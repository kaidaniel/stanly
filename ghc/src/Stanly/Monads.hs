{-# LANGUAGE RecordWildCards #-}
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
import Stanly.Val.Concrete qualified as C (
    if',
    lambda,
    op2,
 )
import Stanly.Val.Value (
    Val,
    Value,
    closure',
    number',
    text',
 )

type ConcreteT val m = EnvT Int (ExcT (StoreT Int (val Int) m))
type AbstractT val m = EnvT Variable (ExcT (StoreT Variable (val Variable) (ListT m)))
type Snapshot val l = StoreRes l (val l) (ExcRes (val l))
type Mixin val l m' m r =
    (Monad m) ⇒ (Interpreter l (val l) (Env l) m' → Eval m' (val l)) → Eval m r
type MixinConcrete val m = Mixin val Int (ConcreteT val m) m (Snapshot val Int)
type MixinAbstract val m =
    Mixin val Variable (AbstractT val m) m (Set (Snapshot val Variable))

concrete ∷ ∀ m. (Monad m) ⇒ MixinConcrete Val m
concrete mixin = runStoreT ∘ runExcT ∘ runEnvT ∘ mixin Interpreter{..}
  where
    lambda = C.lambda
    number = number'
    text = text
    load = \var → lookupₗ var ⇉ lookupStore
    closure = closure'
    bind = \binding cc → local (bind' binding) ⎴ cc
    substitute = \ρ₁ binding cc → local (const (bind' binding ρ₁)) ⎴ cc
    storeₗ = insertStore

    alloc = const (gets len)
    op2 = \o a b → a ⇉ \a₁ → b ⇉ \b₁ → C.op2 o a₁ b₁
    if' = \tst a b → tst ⇉ \tst₁ → C.if' tst₁ a b

abstract ∷ ∀ m. MixinAbstract Val m
abstract mixin = φ fromList ∘ toList ∘ runStoreT ∘ runExcT ∘ runEnvT ∘ mixin Interpreter{..}
  where
    lambda = C.lambda
    number = number'
    text = text'
    load = \var → lookupₗ var ⇉ lookupStore
    closure = closure'
    bind = \binding cc → local (bind' binding) ⎴ cc
    substitute = \ρ₁ binding cc → local (const (bind' binding ρ₁)) ⎴ cc
    storeₗ = insertStore

    alloc = ω
    op2 = \o a b → a ⇉ \a₁ → b ⇉ \b₁ → C.op2 o a₁ b₁
    if' = \tst a b → tst ⇉ \tst₁ → C.if' tst₁ a b
