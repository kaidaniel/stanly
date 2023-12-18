{-# LANGUAGE ScopedTypeVariables #-}

module Stanly.Concrete (runConcrete, evalConcrete) where

import Control.Monad.Except
import Control.Monad.Reader (
    MonadReader (ask, local),
    ReaderT (runReaderT),
 )
import Control.Monad.State
import Data.Coerce
import Data.Maybe (fromMaybe)
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
                { I.deref = \l → get ⇉ ω ∘ \store → fromMaybe (error ⎴ bwText ⎴ " not found in store.\n" ⊹ store) (lookup l ⎴ coerce store)
                , I.exc = exc₁
                , I.env = ask
                , I.alloc = \_ → gets length
                , I.localEnv = local
                , I.store = get
                , I.updateStore = modify
                , I.op2 = \o a b →
                    let exc₂ msg = exc₁ ⎴ ttyText ⎴ msg ⊹ ".\nIn expression: " ⊹ a ⊹ " " ⊹ o ⊹ b
                     in case (a, b) of
                            (I.NumV n₀, I.NumV n₁)
                                | o == "+" → ω ⎴ I.NumV ⎴ n₀ + n₁
                                | o == "-" → ω ⎴ I.NumV ⎴ n₀ - n₁
                                | o == "*" → ω ⎴ I.NumV ⎴ n₀ * n₁
                                | o == "/", n₁ == 0 → exc₂ "Division by zero"
                                | o == "/" → ω ⎴ I.NumV ⎴ div n₀ n₁
                            (I.TxtV t₀, I.TxtV t₁)
                                | o == "+" → ω ⎴ I.TxtV ⎴ t₀ ⋄ t₁
                            (I.TxtV t₀, I.NumV n₁)
                                | o == "+" → ω ⎴ I.TxtV ⎴ t₀ ⋄ show n₁
                            _ → exc₂ "Invalid arguments to operator"
                , I.branch = \fls tru → \case
                    I.NumV n | n == 0 → fls | otherwise → tru
                    _ → exc₁ "Branching on non-numeric value"
                }

op2 o a b = case (a, b) of
    (I.NumV n₀, I.NumV n₁)
        | o == "+" → I.NumV ⎴ n₀ + n₁
        | o == "-" → I.NumV ⎴ n₀ - n₁
        | o == "*" → I.NumV ⎴ n₀ * n₁
        | o == "/", n₁ == 0 → I.Undefined "Division by zero"
        | o == "/" → I.NumV ⎴ div n₀ n₁
    (I.TxtV t₀, I.TxtV t₁)
        | o == "+" → I.TxtV ⎴ t₀ ⋄ t₁
    (I.TxtV t₀, I.NumV n₁)
        | o == "+" → I.TxtV ⎴ t₀ ⋄ show n₁
    _ → I.Undefined "Invalid arguments to operator"
