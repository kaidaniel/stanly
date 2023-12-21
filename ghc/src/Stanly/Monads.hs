{-# LANGUAGE ScopedTypeVariables #-}

module Stanly.Monads (concrete) where

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Reader (MonadReader (local))
import Control.Monad.State (gets)
import Data.Set (Set, fromList)
import ListT (ListT, toList)
import Stanly.Env (EnvT, bind', lookupₗ, runEnvT)
import Stanly.Interpreter (Eval, Interpreter (..))
import Stanly.Language (Expr, Variable)
import Stanly.Store (Store, StoreT, len, lookupᵥ, runStoreT, store')
import Stanly.Unicode
import Stanly.Val (Val, arithmetic, closureᵥ, ifn0)

type ExcT m = ExceptT String m

type ConcreteT m = EnvT Int (ExcT (StoreT Int m))

type Mixin l m = Interpreter l m → Eval l m

type Snapshot l = (Either String (Val l), Store l (Val l))

concrete ∷ ∀ m. (Monad m) ⇒ Mixin Int (ConcreteT m) → Expr → m (Snapshot Int)
concrete mixin = runStoreT ∘ runExceptT ∘ runEnvT ∘ mixin interpreter
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

abstract ∷ ∀ m. (Monad m) ⇒ Mixin Variable (AbstractT m) → Expr → m (Set (Snapshot Variable))
abstract mixin = φ fromList ∘ toList ∘ runStoreT ∘ runExceptT ∘ runEnvT ∘ mixin interpreter
  where
    interpreter =
        Interpreter
            { load = undefined
            , closure = undefined
            , bind = undefined
            , alloc = undefined
            , substitute = undefined
            , storeₗ = undefined
            , op2 = undefined
            , if' = undefined
            }
