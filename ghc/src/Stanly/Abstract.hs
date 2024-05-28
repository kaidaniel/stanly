{-# LANGUAGE UndecidableInstances #-}

module Stanly.Abstract (values, store, runAbstractT, Store (..), Abstracted (..)) where

import Control.Applicative
import Control.Monad.Except as M
import Control.Monad.Reader as M
import Control.Monad.State as M
import Data.Map qualified as Map
import Data.Maybe (catMaybes)
import Data.Set qualified as Set
import ListT
import Stanly.Concrete (concreteIsTruthy, concreteOp2)
import Stanly.Eval as E
import Stanly.Language
import Stanly.Unicode

data Abstracted val
    = Precise (Either E.Exception val)
    | OneOf (Set.Set (Either E.Exception val))
    | Top
    deriving (Eq, Show, Ord)

prune ∷ Abstracted Val → Abstracted Val
prune = \case
    Precise x
        | x == (ω E.Any) → Top
        | otherwise → Precise (φ E.prune x)
    OneOf s
        | (ω E.Any) `Set.member` s → Top
        | otherwise → OneOf (Set.map (\x → φ E.prune x) s)
    Top → Top

instance (Ord val) ⇒ Semigroup (Abstracted val) where
    Precise a <> Precise b
        | a == b = Precise a
        | otherwise = OneOf (Set.singleton a <> Set.singleton b)
    Top <> _ = Top
    _ <> Top = Top
    OneOf a <> OneOf b = OneOf (a <> b)
    Precise a <> OneOf b = OneOf (Set.singleton a <> b)
    OneOf a <> Precise b = OneOf (a <> Set.singleton b)

newtype Store = MkStore (Map.Map E.Loc (Abstracted E.Val))
    deriving (Eq, Semigroup, Monoid, Show, Ord)

type AbstractTRep m = M.ReaderT Env (M.StateT Store (M.ExceptT Exception (ListT m)))

newtype AbstractT m a = AbstractT (Env → Store → ListT m (Either Exception (a, Store)))
    deriving
        (Functor, Applicative, Monad, MonadEnv, MonadExc, M.MonadState Store)
        via AbstractTRep m

instance (Monad m) ⇒ Alternative (AbstractT m) where
    empty = AbstractT \_ _ → ε₁
    (AbstractT f) <|> (AbstractT g) = AbstractT \e s → f e s <> g e s

nubM ∷ ∀ m a. (Monad m, Ord a) ⇒ ListT m a → ListT m a
nubM m1 = ListT.unfoldM f (φ Set.toList ⎴ φ Set.fromList ⎴ ListT.toList m1)
  where
    f ∷ m [a] → m (Maybe (a, m [a]))
    f m2 = do
        els ← m2
        case els of
            [] → ω Nothing
            x : xs → ω ⎴ Just (x, ω xs)

runAbstractT ∷ (Monad m, Ord a) ⇒ AbstractT m a → m [Either Exception (a, Store)]
runAbstractT (AbstractT f) = ListT.toList (nubM (f ε₁ ε₁))

joinStores ∷ [Either Exception (Val, Store)] → Store
joinStores r = γ catS stores
  where
    stores = (catMaybes (map (\case Left _ → Nothing; Right (_, s) → Just s) r))
    catS ∷ [Map.Map Loc (Abstracted Val)] → Map.Map Loc (Abstracted Val)
    catS = \case
        [] → ε₁
        [store1, store2] → Map.unionWith (<>) store1 store2
        x : xs → catS [x, catS xs]

store ∷ [Either Exception (Val, Store)] → Store
store result = let MkStore s = (joinStores result) in MkStore (Map.map Stanly.Abstract.prune s)

values ∷ [Either Exception (Val, Store)] → [Either Exception Val]
values res = Set.toList (Set.fromList [fmap_ π₁ x | x ← res])

truncateV ∷ Val → Val
truncateV =
    let bound = 1000
     in \case
            E.Num n
                | n > bound → E.Any
                | n < -bound → E.Any
                | otherwise → E.Num n
            E.Txt s
                | length s > (fromIntegral bound) → E.Any
                | otherwise → E.Txt s
            _ → E.Any

instance (Monad m) ⇒ Interpreter (AbstractT m) where
    op2 = \cases
        Divide lhs E.Any →
            ω E.Any
                ⫶ throwError (DivisionByZero lhs E.Any)
                ⫶ throwError (InvalidArgsToOperator lhs Divide E.Any)
        o lhs rhs → φ truncateV (concreteOp2 o lhs rhs) ⫶ err
          where
            err
                | lhs == E.Any || rhs == E.Any = throwError (InvalidArgsToOperator lhs o rhs)
                | otherwise = εₐ
    isTruthy = \case
        E.Any → (ω True) ⫶ (ω False) ⫶ throwError (BranchOnNonNumeric E.Any)
        x → concreteIsTruthy x
    find loc = AbstractT \_ s →
        case (γ s) Map.!? loc of
            Just (Precise val) → ω ⎴ do v ← val; ω (v, s)
            Just (OneOf set) → fromFoldable [fmap_ (\x → (x, s)) v | v ← Set.toList set]
            Just (Top) → ω ⎴ ω (E.Any, s)
            Nothing → ω ⎴ throwError (InvalidLoc loc)
    ext loc val =
        M.modify ⎴ γ \s →
            let
                new = Precise (ω val)
                stronglyUpdated = case s Map.!? loc of
                    Nothing → new
                    -- Just (Precise _old) → new
                    Just old → old <> new
             in
                Map.insert loc stronglyUpdated s

    alloc var = ω var
