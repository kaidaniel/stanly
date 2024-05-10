{-# LANGUAGE UndecidableInstances #-}

module Stanly.Abstract where

import Control.Applicative
import Control.Monad as M
import Control.Monad.Except as M
import Control.Monad.Reader as M
import Control.Monad.State as M
import Data.Bits (xor)
import Data.List (foldl')
import Data.Map qualified as Map
import ListT (ListT)
import Stanly.Concrete (concreteIsTruthy, concreteOp2)
import Stanly.Eval
import Stanly.Language
import Stanly.Unicode

type AbstractTRep m = M.ReaderT Env (M.StateT Store (M.ExceptT Exception (ListT m)))

data Extremum = Top | Bottom
newtype AbstractT m a = AbstractT (Env → Store → ListT m (Either Exception (a, Store)))
    deriving
        (Functor, Applicative, Monad, MonadExc, MonadEnv, MonadStore, Alternative, MonadPlus)
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
    find loc =
        AbstractT ⎴ \cases
            _ s
                | Just v ← (γ s) Map.!? loc → ω ⎴ ω (v, s)
                | otherwise → ω ⎴ throwError (InvalidLoc loc s)
    alloc var = ω (foldl' (\h c → 33 * h `xor` fromEnum c) 5381 var) -- DJB2 hashing
