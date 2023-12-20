{-# LANGUAGE ScopedTypeVariables #-}

module Stanly.Monads (concrete) where

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Reader (MonadReader (local))
import Control.Monad.State (gets)
import ListT (ListT (..))
import Stanly.Env (EnvT, bind', lookupₗ, runEnvT)
import Stanly.Interpreter (Eval, Interpreter (..))
import Stanly.Language (Expr, Variable)
import Stanly.Store (Store, StoreT, len, lookupᵥ, runStoreT, store')
import Stanly.Unicode
import Stanly.Val (Val, arithmetic, closureᵥ, ifn0)

type ExcT m = ExceptT String m

type ConcreteT m = EnvT Int (ExcT (StoreT Int m))

type Mixin l m = Interpreter l m → Eval l m

concrete ∷ ∀ m. (Monad m) ⇒ Mixin Int (ConcreteT m) → Expr → m (Either String (Val Int), Store Int)
concrete ev = runStoreT ∘ runExceptT ∘ runEnvT ∘ ev interpreter
  where
    interpreter =
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

type AbstractT m = EnvT Variable (ExcT (StoreT Variable (ListT m)))

abstract ∷ ∀ m. (Monad m) ⇒ Mixin Variable (AbstractT m) → Expr → m (Either String (Val Variable), Store Variable)
abstract = undefined
