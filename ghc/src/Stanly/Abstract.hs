{-# LANGUAGE UndecidableInstances #-}

module Stanly.Abstract where

import Control.Applicative
import Control.Monad as M
import Control.Monad.Except as M
import Control.Monad.Reader as M
import Control.Monad.State as M
import Data.Map qualified as Map
import Data.Set qualified as Set
import ListT (ListT)
import Stanly.Concrete (concreteIsTruthy, concreteOp2)
import Stanly.Eval
import Stanly.Language
import Stanly.Unicode

data Abstracted val
    = Precise (Either Exception val)
    | OneOf (Set.Set (Either Exception val))
    | Top
    | Bottom
    deriving (Eq, Show)

instance (Ord val) ⇒ Semigroup (Abstracted val) where
    Precise a <> Precise b = Precise (a <> b)
    Top <> _ = Top
    _ <> Top = Top
    Bottom <> x = x
    x <> Bottom = x
    OneOf a <> OneOf b = OneOf (Set.union a b)
    Precise a <> OneOf b = OneOf (Set.singleton a <> b)
    OneOf a <> Precise b = OneOf (a <> Set.singleton b)

instance (Ord val) ⇒ Monoid (Abstracted val) where
    mempty = Bottom

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
        , Alternative
        , MonadPlus
        )
        via AbstractTRep m

truncateV ∷ Val → Val
truncateV = \case
    NumV n
        | n > 100 → TopV
        | n < -100 → TopV
        | otherwise → NumV n
    TxtV s
        | length s > 100 → TopV
        | otherwise → TxtV s
    _ → TopV

instance (Monad m) ⇒ Interpreter (AbstractT m) where
    op2 = \cases
        Divide lhs TopV →
            ω TopV
                ⫶ throwError (DivisionByZero lhs TopV)
                ⫶ throwError (InvalidArgsToOperator lhs Divide TopV)
        o lhs rhs → φ truncateV (concreteOp2 o lhs rhs) ⫶ err
          where
            err
                | lhs == TopV || rhs == TopV = throwError (InvalidArgsToOperator lhs o rhs)
                | otherwise = εₘ
    isTruthy = \case
        TopV → (ω True) ⫶ (ω False) ⫶ throwError (BranchOnNonNumeric TopV)
        x → concreteIsTruthy x
    find loc = AbstractT \_ s →
        case (γ s) Map.!? loc of
            Just (vs ∷ Abstracted Val) → undefined
            Nothing → ω ⎴ throwError (InvalidLoc loc)
    ext loc val = M.modify ⎴ γ \s → Map.insertWith (<>) loc (Precise ⎴ ω val) s
    alloc var = ω var
