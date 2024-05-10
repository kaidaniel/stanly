{-# LANGUAGE UndecidableInstances #-}

module Stanly.Concrete (runConcreteT, ConcreteT, concreteOp2, concreteIsTruthy) where

import Control.Monad.Except qualified as M
import Control.Monad.Reader qualified as M
import Control.Monad.State qualified as M
import Control.Monad.Writer qualified as M
import Data.Map (size, (!?))
import Stanly.Eval
import Stanly.Language
import Stanly.Unicode

type ConcreteTRep m = M.ReaderT Env (M.StateT Store (M.ExceptT Exception m))
newtype ConcreteT m a = ConcreteT (Env → Store → m (Either Exception (a, Store)))
    deriving (Functor, Applicative, Monad, MonadExc, MonadEnv, MonadStore) via ConcreteTRep m

deriving via (ConcreteTRep m) instance (M.MonadWriter w m) ⇒ M.MonadWriter w (ConcreteT m)

runConcreteT ∷ ConcreteT m a → m (Either Exception (a, Store))
runConcreteT (ConcreteT f) = f ε₁ ε₁

concreteOp2 ∷ (MonadExc m) ⇒ Op2 → Val → Val → m Val
concreteOp2 o lhs rhs = do
    case (lhs, rhs) of
        (NumV n₀, NumV n₁)
            | o == Plus → ω ⎴ NumV ⎴ n₀ + n₁
            | o == Minus → ω ⎴ NumV ⎴ n₀ - n₁
            | o == Times → ω ⎴ NumV ⎴ n₀ * n₁
            | o == Divide, n₁ == 0 → M.throwError (DivisionByZero lhs rhs)
            | o == Divide → ω ⎴ NumV ⎴ div n₀ n₁
        (TxtV t₀, TxtV t₁)
            | o == Plus → ω ⎴ TxtV ⎴ t₀ ⋄ t₁
        (TxtV t₀, NumV n₁)
            | o == Plus → ω ⎴ TxtV ⎴ t₀ ⋄ show n₁
        (TopV, _) → ω TopV
        (_, TopV) → ω TopV
        _ → M.throwError (InvalidArgsToOperator lhs o rhs)

concreteIsTruthy ∷ (MonadExc m) ⇒ Val → m Bool
concreteIsTruthy = \case
    NumV n → ω (n /= 0)
    TxtV s → ω (length s /= 0)
    x → M.throwError (BranchOnNonNumeric x)

instance (Monad m) ⇒ Interpreter (ConcreteT m) where
    op2 = concreteOp2
    isTruthy = concreteIsTruthy
    find loc = ConcreteT \_ (MkStore s) →
        ω ⎴ case s !? loc of
            Just v → ω (v, (MkStore s))
            Nothing → M.throwError (InvalidLoc loc (MkStore s))
    alloc _ = do
        (MkStore s) ← M.get
        pure (size s)
