module Stanly.Monads (concrete, value, store) where

import Control.Monad.Reader (MonadReader (local))
import Control.Monad.State (gets)
import Data.Set (Set, fromList)
import ListT (ListT, toList)
import Stanly.Env (Env, EnvT, bind', lookupₗ, runEnvT)
import Stanly.Exc (ExcRes, ExcT, runExcT)
import Stanly.Fmt (Fmt)
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
import Stanly.Val.Abstract qualified as A (
    ValA,
    if',
    lambda,
    op2,
 )
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

type InterpreterT val loc m = EnvT loc (ExcT (StoreT loc (val loc) m))
type MkEval val l m t resT =
    (Monad m, Value val, Show (val l), Fmt (val l)) ⇒
    ( Interpreter l (val l) (Env l) (InterpreterT val l (t m)) →
      Eval (InterpreterT val l (t m)) (val l)
    ) →
    Eval m (resT (StoreRes l (val l) (ExcRes (val l))))
type Id a = a

concrete ∷ ∀ m. MkEval Val Int m Id Id
concrete mixin = runStoreT ∘ runExcT ∘ runEnvT ∘ mixin Interpreter{..}
  where
    lambda = C.lambda
    number = number'
    text = text'
    load = \var → lookupₗ var ⇉ lookupStore
    closure = closure'
    bind = \binding cc → local (bind' binding) ⎴ cc
    substitute = \ρ₁ binding cc → local (const (bind' binding ρ₁)) ⎴ cc
    storeₗ = insertStore

    alloc = const (gets len)
    op2 = \o a b → a ⇉ \a₁ → b ⇉ \b₁ → C.op2 o a₁ b₁
    if' = \tst a b → tst ⇉ \tst₁ → C.if' tst₁ a b

abstract ∷ ∀ m. MkEval A.ValA Variable m ListT Set
abstract mixin = φ fromList ∘ toList ∘ runStoreT ∘ runExcT ∘ runEnvT ∘ mixin Interpreter{..}
  where
    lambda = A.lambda
    number = number'
    text = text'
    load = \var → lookupₗ var ⇉ lookupStore
    closure = closure'
    bind = \binding cc → local (bind' binding) ⎴ cc
    substitute = \ρ₁ binding cc → local (const (bind' binding ρ₁)) ⎴ cc
    storeₗ = insertStore

    alloc = ω
    op2 = \o a b → a ⇉ \a₁ → b ⇉ \b₁ → A.op2 o a₁ b₁
    if' = \tst a b → tst ⇉ \tst₁ → A.if' tst₁ a b
