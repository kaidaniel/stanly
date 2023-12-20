{-# LANGUAGE ScopedTypeVariables #-}

module Stanly.Concrete (ConcreteT, runConcreteT) where

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Reader (MonadReader (local))
import Control.Monad.State (gets)
import Stanly.Env (EnvT, bind', lookupₗ, runEnvT)
import Stanly.Interpreter (Eval, Interpreter (..))
import Stanly.Language (Expr)
import Stanly.Store (Store, StoreT, len, lookupᵥ, runStoreT, store')
import Stanly.Unicode
import Stanly.Val (Val, arithmetic, closureᵥ, ifn0)

type ExcT m = ExceptT String m

type ConcreteT m = ExcT (EnvT Int (StoreT Int m))

runConcreteT ∷ ∀ m. (Monad m) ⇒ (Interpreter Int (ConcreteT m) → Eval Int (ConcreteT m)) → Expr → m (Either String (Val Int), Store Int)
runConcreteT ev = runStoreT ∘ runEnvT ∘ runExceptT ∘ ev concreteInterpreter
  where
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
