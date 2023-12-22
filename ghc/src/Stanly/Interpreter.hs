{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}

module Stanly.Interpreter (Interpreter (..), makeInterpreter, liftInterpreter, Eval) where

import Control.Monad.Except (MonadError)
import Control.Monad.Trans (MonadTrans, lift)
import Data.Function (fix)
import Stanly.Env (Env)
import Stanly.Fmt (Fmt)
import Stanly.Language (Expr (..), Op2, Variable)
import Stanly.Unicode
import Stanly.Val (Val, lambda, number, text)

type Eval l m = Expr → m (Val l)

makeInterpreter ∷ ∀ l m. (Eval l m → Eval l m) → ((Eval l m → Eval l m) → (Eval l m → Eval l m)) → Interpreter l m → Eval l m
makeInterpreter closed open interpreter = closed ⎴ fix ⎴ open ⎴ eval interpreter

eval ∷ ∀ m l. Interpreter l m → Eval l m → Eval l m
eval Interpreter{..} eval₁ = \case
    Num n → number n
    Txt s → text s
    Lam x e → closure x e
    Var x → load x
    If' tst then' else' → if' (eval₁ tst) (eval₁ then') (eval₁ else')
    Op2 o e₁ e₂ → op2 o (eval₁ e₁) (eval₁ e₂)
    Rec var body → do
        loc ← alloc var
        value ← bind (var, loc) (eval₁ body)
        storeₗ (loc, value)
        ω value
    App f x → do
        (var, body, ρ₁) ← eval₁ f ⇉ lambda
        x₁ ← eval₁ x
        loc ← alloc var
        storeₗ (loc, x₁)
        substitute ρ₁ (var, loc) (eval₁ body)

data Interpreter l m where
    Interpreter ∷
        (Fmt l, Monad m, MonadError String m) ⇒
        { load ∷ Variable → m (Val l)
        , closure ∷ Variable → Expr → m (Val l)
        , bind ∷ (Variable, l) → m (Val l) → m (Val l)
        , substitute ∷ Env l → (Variable, l) → m (Val l) → m (Val l)
        , storeₗ ∷ (l, Val l) → m ()
        , alloc ∷ Variable → m l
        , op2 ∷ Op2 → m (Val l) → m (Val l) → m (Val l)
        , if' ∷ m (Val l) → m (Val l) → m (Val l) → m (Val l)
        } →
        Interpreter l m

liftInterpreter ∷ ∀ l m t. (MonadError String (t m), MonadTrans t, Monad (t m)) ⇒ Interpreter l m → Interpreter l (t m)
liftInterpreter Interpreter{..} =
    Interpreter
        { load = ζ₀ ∘ load
        , closure = \v → ζ₀ ∘ closure v
        , bind = ζ₁ ∘ bind
        , substitute = \ρ → ζ₁ ∘ substitute ρ
        , storeₗ = ζ₀ ∘ storeₗ
        , alloc = ζ₀ ∘ alloc
        , op2 = ζ₂ ∘ op2
        , if' = ζ₃ if'
        }
  where
    ζ₀ ∷ m r → t m r
    ζ₁ ∷ (m x₁ → m r) → (t m x₁ → t m r)
    ζ₂ ∷ (m x₁ → m x₂ → m r) → (t m x₁ → t m x₂ → t m r)
    ζ₃ ∷ (m x₁ → m x₂ → m x₃ → m r) → (t m x₁ → t m x₂ → t m x₃ → t m r)

    f ⊰ a = f ⊛ φ ω a
    infixl 4 ⊰
    ζ₀ = lift
    ζ₁ f x = ω f ⊰ x ⇉ ζ₀
    ζ₂ f x y = ω f ⊰ x ⊰ y ⇉ ζ₀
    ζ₃ f x y z = ω f ⊰ x ⊰ y ⊰ z ⇉ ζ₀
