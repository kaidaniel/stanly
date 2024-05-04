{-# LANGUAGE UndecidableInstances #-}

module Stanly.Concrete (runConcrete, runConcreteT, ConcreteT) where

import Control.Monad.Except qualified as M
import Control.Monad.Identity qualified as M
import Control.Monad.Reader qualified as M
import Control.Monad.State qualified as M
import Control.Monad.Writer qualified as M
import Data.Map (size, (!?))
import Stanly.Eval (
    Env (..),
    Exception (..),
    Interpreter (..),
    Store (..),
    Val (..),
 )
import Stanly.Language (Op2 (..))
import Stanly.Unicode

type ConcreteTVia m = M.ReaderT Env (M.StateT Store (M.ExceptT Exception m))
newtype ConcreteT m a = ConcreteT (Env → Store → m (Either Exception (a, Store)))
    deriving
        ( Functor
        , Applicative
        , Monad
        , M.MonadError Exception
        , M.MonadReader Env
        , M.MonadState Store
        )
        via ConcreteTVia m

instance ∀ w m. (M.MonadWriter w m) ⇒ M.MonadWriter w (ConcreteT m) where
    tell ∷ w → ConcreteT m ()
    tell = M.lift ∘ M.tell
    listen ∷ ∀ a. ConcreteT m a → ConcreteT m (a, w)
    listen ma = γ (M.listen (γ ma ∷ ConcreteTVia m a))
    pass ∷ ∀ a. ConcreteT m (a, w → w) → ConcreteT m a
    pass m = γ ⎴ M.pass (γ m ∷ ConcreteTVia m (a, w → w))

instance M.MonadTrans ConcreteT where
    lift ∷ ∀ m a. (Monad m) ⇒ m a → ConcreteT m a
    lift = γ @(ConcreteTVia _ _) ∘ M.lift ∘ M.lift ∘ M.lift

runConcreteT ∷ ConcreteT m a → m (Either Exception (a, Store))
runConcreteT (ConcreteT f) = f ε₁ ε₁

runConcrete ∷ ConcreteT M.Identity a → Either Exception (a, Store)
runConcrete = M.runIdentity ∘ runConcreteT

instance (Monad m) ⇒ Interpreter (ConcreteT m) where
    op2 o lhs rhs = do
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
            _ → M.throwError (InvalidArgsToOperator lhs o rhs)
    if' tst then' else' = do
        tst' ← tst
        case tst' of
            NumV n | n == 0 → else' | otherwise → then'
            _ → M.throwError (BranchOnNonNumeric tst')
    find loc = γ \(_ ∷ Env) s → case s !? loc of
        Just v → ω @m (Right (v, (Store s)))
        Nothing → ω @m (Left ⎴ InvalidLoc loc (Store s))
    alloc _ = do
        Store s ← M.get
        pure (size s)
