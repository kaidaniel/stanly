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
        I.Interpreter
            { I.deref = \l → get ⇉ ω ∘ \store → (error ⎴ bwText ⎴ " not found in store.\n" ⊹ store) `fromMaybe` (lookup l ⎴ coerce store)
            , I.exc = throwError
            , I.env = ask
            , I.alloc = const (gets length)
            , I.localEnv = local
            , I.store = get
            , I.updateStore = modify
            , I.op2 = I.arithmetic
            , I.branch = I.branchIfn0
            }
