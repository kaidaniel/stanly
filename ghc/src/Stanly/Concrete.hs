{-# LANGUAGE ScopedTypeVariables #-}

module Stanly.Concrete (runConcrete, evalConcrete) where

import Control.Monad.Except
import Control.Monad.Reader (
    MonadReader (ask, local),
    ReaderT (runReaderT),
 )
import Control.Monad.State
import Stanly.Fmt
import Stanly.Interpreter qualified as I
import Stanly.Unicode

type EnvT m = ReaderT (I.Env Int) m
type StoreT m = StateT (I.Store Int) m
type ExcT m = ExceptT String m

type ConcreteT m = ExcT (EnvT (StoreT m))

evalConcrete ∷ ∀ m. (Monad m) ⇒ I.Combinator Int (ConcreteT m) → I.Expr → m (Either String (I.Val Int))
evalConcrete c e = runConcrete c e ⇉ \x → ω ⎴ π₁ x

runConcrete ∷ ∀ m. (Monad m) ⇒ I.Combinator Int (ConcreteT m) → I.Expr → m (Either String (I.Val Int), I.Store Int)
runConcrete ev = rstore ∘ renv ∘ rexc ∘ ev₁
  where
    rstore = flip runStateT ε₁
    renv = flip runReaderT ε₁
    rexc = runExceptT
    ev₁ = ev concreteInterpreter
    concreteInterpreter =
        let exc₁ er = throwError ⎴ "Exception: " ⋄ er
         in I.Interpreter
                { I.deref = \l → do
                    I.Store store ← get
                    case lookup l store of
                        Just val → ω val
                        Nothing → error ⎴ show l ⋄ " not found in store. " ⋄ bwText (I.Store store)
                , I.exc = exc₁
                , I.env = ask
                , I.alloc = \_ → gets length
                , I.localEnv = local
                , I.store = get
                , I.updateStore = modify
                , I.op2 = \o a b → case (o, a, b) of
                    ("+", I.NumV n₀, I.NumV n₁) → ω ∘ I.NumV ⎴ n₀ + n₁
                    ("-", I.NumV n₀, I.NumV n₁) → ω ∘ I.NumV ⎴ n₀ - n₁
                    ("*", I.NumV n₀, I.NumV n₁) → ω ∘ I.NumV ⎴ n₀ * n₁
                    ("/", I.NumV n₀, I.NumV n₁) →
                        if n₁ == 0
                            then exc₁ ⎴ "Division by zero. " ⋄ show n₀ ⋄ "/" ⋄ show n₁
                            else ω ⎴ I.NumV ⎴ div n₀ n₁
                    ("+", I.TxtV t₀, I.TxtV t₁) → ω ∘ I.TxtV ⎴ t₀ ⋄ t₁
                    ("+", I.TxtV t₀, I.NumV n₁) → ω ∘ I.TxtV ⎴ t₀ ⋄ show n₁
                    _ → exc₁ ⎴ invalidOperands o a b
                , I.branch = \fls tru → \case
                    I.NumV n → if n /= 0 then tru else fls
                    _ → ω ⎴ I.Undefined "Branching on non-numeric value"
                }

invalidOperands ∷ (Fmt a₁, Fmt a₂) ⇒ String → a₁ → a₂ → String
invalidOperands o a b =
    "Invalid arguments to operator '"
        ⋄ o
        ⋄ "':\n"
        ⋄ "\nleft operand  ⋙ "
        ⋄ ttyText a
        ⋄ "\noperation     ⋙ "
        ⋄ o
        ⋄ "\nright operand ⋙ "
        ⋄ ttyText b
