{-# LANGUAGE ScopedTypeVariables #-}

module Stanly.Concrete (runConcrete) where

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Reader (MonadReader (local), ReaderT (runReaderT))
import Control.Monad.State (StateT (runStateT), gets)
import Stanly.Env (Env, bind', lookupₗ)
import Stanly.Interpreter (Eval, Interpreter (..))
import Stanly.Language (Expr)
import Stanly.Store (Store, len, lookupᵥ, store')
import Stanly.Unicode
import Stanly.Val (Val, arithmetic, closureᵥ, ifn0)

type EnvT m = ReaderT (Env Int) m
type StoreT m = StateT (Store Int) m
type ExcT m = ExceptT String m

type ConcreteT m = ExcT (EnvT (StoreT m))

runConcrete ∷ ∀ m. (Monad m) ⇒ (Interpreter Int (ConcreteT m) → Eval Int (ConcreteT m)) → Expr → m (Either String (Val Int), Store Int)
runConcrete ev = rstore ∘ renv ∘ rexc ∘ ev₁
  where
    rstore = flip runStateT ε₁
    renv = flip runReaderT ε₁
    rexc = runExceptT
    ev₁ = ev concreteInterpreter
    concreteInterpreter =
        Interpreter
            { load = \var → lookupₗ var ⇉ lookupᵥ
            , closure = closureᵥ
            , bind = \binding cc → local (bind' binding) ⎴ cc
            , alloc = const (gets len)
            , substitute = \ρ₁ binding cc → local (const (bind' binding ρ₁)) ⎴ cc
            , storeₗ = store'
            , op2 = \o a b → a ⇉ \a₁ → b ⇉ \b₁ → arithmetic o a₁ b₁
            , if' = \tst a b → tst ⇉ \tst₁ → ifn0 tst₁ a b
            }
