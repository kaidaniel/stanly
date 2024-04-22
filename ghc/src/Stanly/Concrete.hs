module Stanly.Concrete (store, value) where

import Control.Monad.Except (MonadError (..))
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.Trans (MonadTrans (..))
import Data.Map (insert, size, (!?))
import Stanly.Eval (
    Env (..),
    Exception (..),
    Interpreter (..),
    InterpreterT (..),
    Res (..),
    Store (..),
    Val (..),
    interpreter,
 )
import Stanly.Language (Op2 (..))
import Stanly.Unicode

newtype ConcreteT m a = ConcreteT (InterpreterT m a)
    deriving (Functor, Applicative, Monad, MonadError Exception, MonadTrans)

store ∷ ConcreteT Identity a → Either Store Exception
store (ConcreteT (InterpreterT f)) = case runIdentity (f ε₁ ε₁) of
    Step _ s → Left s
    Stop exc → Right exc

value ∷ ConcreteT Identity a → Either a Exception
value (ConcreteT (InterpreterT f)) = case runIdentity (f ε₁ ε₁) of
    Step v _ → Left v
    Stop exc → Right exc

concrete ∷ (Monad m) ⇒ (Env → Store → m a) → ConcreteT m a
concrete f = ConcreteT (interpreter f)

instance (Monad m) ⇒ Interpreter (ConcreteT m) where
    op2 o lhs rhs = do
        case (lhs, rhs) of
            (NumV n₀, NumV n₁)
                | o == Plus → ω ⎴ NumV ⎴ n₀ + n₁
                | o == Minus → ω ⎴ NumV ⎴ n₀ - n₁
                | o == Times → ω ⎴ NumV ⎴ n₀ * n₁
                | o == Divide, n₁ == 0 → throwError (DivisionByZero lhs rhs)
                | o == Divide → ω ⎴ NumV ⎴ div n₀ n₁
            (TxtV t₀, TxtV t₁)
                | o == Plus → ω ⎴ TxtV ⎴ t₀ ⋄ t₁
            (TxtV t₀, NumV n₁)
                | o == Plus → ω ⎴ TxtV ⎴ t₀ ⋄ show n₁
            _ → throwError (InvalidArgsToOperator lhs o rhs)
    if' tst then' else' = do
        tst' ← tst
        case tst' of
            NumV n | n == 0 → else' | otherwise → then'
            _ → throwError (BranchOnNonNumeric tst')
    env = concrete (\e _ → ω e)
    inEnv ρ = γ \m₁ (_ ∷ Env) s → m₁ ρ (Store s) ∷ m (Res Val)
    update (loc, val) = γ \(_ ∷ Env) s → ω @m (Step () (Store ⎴ insert loc val s))
    find loc = γ \(_ ∷ Env) s → case s !? loc of
        Just v → ω @m (Step v (Store s))
        Nothing → ω @m (Stop ⎴ InvalidLoc loc (Store s))
    alloc _ = concrete (\_ (Store s) → ω (size s))
