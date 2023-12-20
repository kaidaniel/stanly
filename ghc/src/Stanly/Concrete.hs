{-# LANGUAGE ScopedTypeVariables #-}

module Stanly.Concrete (runConcrete, evalConcrete) where

import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.Reader (MonadReader (ask, local), ReaderT (runReaderT))
import Control.Monad.State (MonadState (get), StateT (runStateT), gets, modify)
import Data.Coerce (coerce)
import Data.Maybe (fromMaybe)
import Stanly.Fmt (bwText, (⊹))
import Stanly.Interpreter qualified as I
import Stanly.Language (Expr)
import Stanly.MachineState (Env, Store (..), Val)
import Stanly.Unicode

type EnvT m = ReaderT (Env Int) m
type StoreT m = StateT (Store Int) m
type ExcT m = ExceptT String m

type ConcreteT m = ExcT (EnvT (StoreT m))

evalConcrete ∷ ∀ m. (Monad m) ⇒ (I.Interpreter Int (ConcreteT m) → I.Eval Int (ConcreteT m)) → Expr → m (Either String (Val Int))
evalConcrete c e = runConcrete c e ⇉ \x → ω ⎴ π₁ x

runConcrete ∷ ∀ m. (Monad m) ⇒ (I.Interpreter Int (ConcreteT m) → I.Eval Int (ConcreteT m)) → Expr → m (Either String (Val Int), Store Int)
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
