{-# LANGUAGE UndecidableInstances #-}

module Stanly.Abstract where

import Control.Applicative
import Control.Monad.Except as M
import Control.Monad.Reader as M
import Control.Monad.State as M
import Data.Map qualified as Map
import Data.Set qualified as Set
import ListT (ListT (..), fromFoldable, toList)
import Stanly.Concrete (concreteIsTruthy, concreteOp2)
import Stanly.Eval
import Stanly.Language
import Stanly.Unicode

data Abstracted val
    = Precise (Either Exception val)
    | OneOf (Set.Set (Either Exception val))
    | Top
    deriving (Eq, Show)

instance (Ord val) ⇒ Semigroup (Abstracted val) where
    Precise a <> Precise b = OneOf (Set.singleton a <> Set.singleton b)
    Top <> _ = Top
    _ <> Top = Top
    OneOf a <> OneOf b = OneOf (a <> b)
    Precise a <> OneOf b = OneOf (Set.singleton a <> b)
    OneOf a <> Precise b = OneOf (a <> Set.singleton b)

newtype Store = MkStore (Map.Map Loc (Abstracted Val))
    deriving (Eq, Semigroup, Monoid, Show)

type AbstractTRep m = M.ReaderT Env (M.StateT Store (M.ExceptT Exception (ListT m)))

newtype AbstractT m a = AbstractT (Env → Store → ListT m (Either Exception (a, Store)))
    deriving
        ( Functor
        , Applicative
        , Monad
        , MonadExc
        , MonadEnv
        , M.MonadState Store
        )
        via AbstractTRep m

instance (Monad m) ⇒ Alternative (AbstractT m) where
    empty = AbstractT \_ _ → ε₁
    (AbstractT f) <|> (AbstractT g) = AbstractT \e s → f e s <> g e s

runAbstractT ∷ (Monad m) ⇒ AbstractT m a → m [Either Exception (a, Store)]
runAbstractT (AbstractT f) = ListT.toList (f ε₁ ε₁)

joinedStore ∷ (Monad m) ⇒ AbstractT m a → m Store
joinedStore m = fmap κ₁ stores
  where
    stores = do
        x ← runAbstractT m
        pure do
            li ← x
            case li of
                Left _ → ε₁
                Right (_, store) → ω store

values ∷ (Monad m) ⇒ AbstractT m Val → m [Val]
values m = do
    x ← runAbstractT m
    pure do
        li ← x
        case li of
            Left _ → ε₁
            Right (val, _) → ω val

truncateV ∷ Val → Val
truncateV = \case
    NumV n
        | n > 100 → AnyV
        | n < -100 → AnyV
        | otherwise → NumV n
    TxtV s
        | length s > 100 → AnyV
        | otherwise → TxtV s
    _ → AnyV

instance (Monad m) ⇒ Interpreter (AbstractT m) where
    op2 = \cases
        Divide lhs AnyV →
            ω AnyV
                ⫶ throwError (DivisionByZero lhs AnyV)
                ⫶ throwError (InvalidArgsToOperator lhs Divide AnyV)
        o lhs rhs → φ truncateV (concreteOp2 o lhs rhs) ⫶ err
          where
            err
                | lhs == AnyV || rhs == AnyV = throwError (InvalidArgsToOperator lhs o rhs)
                | otherwise = εₐ
    isTruthy = \case
        AnyV → (ω True) ⫶ (ω False) ⫶ throwError (BranchOnNonNumeric AnyV)
        x → concreteIsTruthy x
    find loc = AbstractT \_ s →
        case (γ s) Map.!? loc of
            Just (Precise val) → ω ⎴ do v ← val; ω (v, s)
            Just (OneOf set) → fromFoldable [fmap_ (\x → (x, s)) v | v ← Set.toList set]
            Just (Top) → ω ⎴ ω (AnyV, s)
            Nothing → ω ⎴ throwError (InvalidLoc loc)
    ext loc val =
        M.modify ⎴ γ \s →
            let
                stronglyUpdated = case s Map.!? loc of
                    Just (Precise _old) → Precise (ω val)
                    Just old → old <> (Precise (ω val))
                    Nothing → Precise (ω val)
             in
                Map.insert loc stronglyUpdated s

    alloc var = ω var
