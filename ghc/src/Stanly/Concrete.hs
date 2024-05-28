{-# LANGUAGE UndecidableInstances #-}

module Stanly.Concrete (runT, ConcreteT, concreteOp2, concreteIsTruthy, Store (..), store) where

import Control.Monad.Except qualified as M
import Control.Monad.Reader qualified as M
import Control.Monad.State qualified as M
import Control.Monad.Writer qualified as M
import Data.Map qualified as Map
import Stanly.Eval as E
import Stanly.Language
import Stanly.Unicode

newtype Store = MkStore (Map.Map Loc Val)
    deriving (Eq, Semigroup, Monoid, Show)

type ConcreteTRep m = M.ReaderT Env (M.StateT Store (M.ExceptT Exception m))
newtype ConcreteT m a = ConcreteT (Env → Store → m (Either Exception (a, Store)))
    deriving
        (Functor, Applicative, Monad, MonadExc, MonadEnv, M.MonadState Store)
        via ConcreteTRep m

deriving via (ConcreteTRep m) instance (M.MonadWriter w m) ⇒ M.MonadWriter w (ConcreteT m)

runT ∷ ConcreteT m a → m (Either Exception (a, Store))
runT (ConcreteT f) = f ε₁ ε₁

store ∷ Either Exception (Val, Store) → Either Exception Store
store result = φ (\(MkStore s) → (MkStore ⎴ Map.map E.prune s)) (φ π₂ result)

concreteOp2 ∷ (MonadExc m) ⇒ Op2 → Val → Val → m Val
concreteOp2 o lhs rhs = do
    case (lhs, rhs) of
        (E.Num n₀, E.Num n₁)
            | o == Plus → ω ⎴ E.Num ⎴ n₀ + n₁
            | o == Minus → ω ⎴ E.Num ⎴ n₀ - n₁
            | o == Times → ω ⎴ E.Num ⎴ n₀ * n₁
            | o == Divide, n₁ == 0 → M.throwError (DivisionByZero lhs rhs)
            | o == Divide → ω ⎴ E.Num ⎴ div n₀ n₁
        (E.Txt t₀, E.Txt t₁)
            | o == Plus → ω ⎴ E.Txt ⎴ t₀ ⋄ t₁
        (E.Txt t₀, E.Num n₁)
            | o == Plus → ω ⎴ E.Txt ⎴ t₀ ⋄ show n₁
        (E.Any, _) → ω E.Any
        (_, E.Any) → ω E.Any
        _ → M.throwError (InvalidArgsToOperator lhs o rhs)

concreteIsTruthy ∷ (MonadExc m) ⇒ Val → m Bool
concreteIsTruthy = \case
    E.Num n → ω (n /= 0)
    E.Txt s → ω (length s /= 0)
    x → M.throwError (BranchOnNonNumeric x)

instance (Monad m) ⇒ Interpreter (ConcreteT m) where
    op2 = concreteOp2
    isTruthy = concreteIsTruthy
    find loc = ConcreteT \_ (MkStore s) →
        ω ⎴ case s Map.!? loc of
            Just v → ω (v, (MkStore s))
            Nothing → M.throwError (InvalidLoc loc)
    ext loc val = M.modify ⎴ γ \s → Map.insert loc val s
    alloc _ = do
        (MkStore s) ← M.get
        pure (show ⎴ Map.size s)
