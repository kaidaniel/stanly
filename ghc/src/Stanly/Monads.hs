{-# LANGUAGE ScopedTypeVariables #-}

module Stanly.Monads (concrete, value, store) where

import Control.Monad.Reader (MonadReader (local))
import Control.Monad.State (gets)
import Data.Set (Set, fromList)
import ListT (ListT, toList)
import Stanly.Env (Env, EnvT, bind', lookupₗ, runEnvT)
import Stanly.Exc (ExcRes, ExcT, runExcT)
import Stanly.Interpreter (Eval, Interpreter (..))
import Stanly.Language (Variable)
import Stanly.Store (
    StoreRes,
    StoreT,
    insertStore,
    len,
    lookupStore,
    runStoreT,
    store,
    value,
 )
import Stanly.Unicode
import Stanly.Val (
    Val,
    arithmetic,
    closureᵥ,
    flattenedArithmetic,
    ifn0,
    lambdaᵥ,
    numberᵥ,
    textᵥ,
 )

type ConcreteT m = EnvT Int (ExcT (StoreT Int m))
type Mixin l m = Interpreter l (Val l) (Env l) m → Eval m (Val l)
type Snapshot l = StoreRes l (ExcRes (Val l))

concrete ∷ ∀ m. (Monad m) ⇒ Mixin Int (ConcreteT m) → Eval m (Snapshot Int)
concrete mixin = runStoreT ∘ runExcT ∘ runEnvT ∘ mixin interpreter
  where
    interpreter =
        Interpreter
            { lambda = lambdaᵥ
            , number = numberᵥ
            , text = textᵥ
            , load = \var → lookupₗ var ⇉ lookupStore
            , closure = closureᵥ
            , bind = \binding cc → local (bind' binding) ⎴ cc
            , alloc = const (gets len)
            , substitute = \ρ₁ binding cc → local (const (bind' binding ρ₁)) ⎴ cc
            , storeₗ = insertStore
            , op2 = \o a b → a ⇉ \a₁ → b ⇉ \b₁ → arithmetic o a₁ b₁
            , if' = \tst a b → tst ⇉ \tst₁ → ifn0 tst₁ a b
            }

type AbstractT m = EnvT Variable (ExcT (StoreT Variable (ListT m)))
abstract ∷
    ∀ m. (Monad m) ⇒ Mixin Variable (AbstractT m) → Eval m (Set (Snapshot Variable))
abstract mixin = φ fromList ∘ toList ∘ runStoreT ∘ runExcT ∘ runEnvT ∘ mixin interpreter
  where
    interpreter =
        Interpreter
            { lambda = lambdaᵥ
            , number = numberᵥ
            , text = textᵥ
            , load = \var → lookupₗ var ⇉ lookupStore
            , closure = closureᵥ
            , bind = \binding cc → local (bind' binding) ⎴ cc
            , alloc = ω
            , substitute = \ρ₁ binding cc → local (const (bind' binding ρ₁)) ⎴ cc
            , storeₗ = insertStore
            , op2 = \o a b → a ⇉ \a₁ → b ⇉ \b₁ → arithmetic o a₁ b₁
            , if' = \tst a b → tst ⇉ \tst₁ → ifn0 tst₁ a b
            }
