{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}

module Stanly.Interpreter (Interpreter (..), makeInterpreter, liftInterpreter, Eval) where

import Control.Monad.Trans (MonadTrans, lift)
import Data.Function (fix)
import Stanly.Exc (MonadExc)
import Stanly.Fmt (Fmt)
import Stanly.Language (Expr (..), Op2, Variable)
import Stanly.Unicode

type Eval m ν = Expr → m ν

makeInterpreter ∷ ∀ ν ρ m. (Eval m ν → Eval m ν) → ((Eval m ν → Eval m ν) → (Eval m ν → Eval m ν)) → Interpreter ν ρ m → Eval m ν
makeInterpreter closed open interpreter = closed ⎴ fix ⎴ open ⎴ eval interpreter

eval ∷ ∀ ν ρ m. Interpreter ν ρ m → Eval m ν → Eval m ν
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
        (var, body, ρ) ← eval₁ f ⇉ lambda
        x₁ ← eval₁ x
        loc ← alloc var
        storeₗ (loc, x₁)
        substitute ρ (var, loc) (eval₁ body)

data Interpreter ν ρ m where
    Interpreter ∷
        (Fmt l, Monad m, MonadExc m) ⇒
        { lambda ∷ ν → m (Variable, Expr, ρ)
        , number ∷ Integer → m ν
        , text ∷ String → m ν
        , load ∷ Variable → m ν
        , closure ∷ Variable → Expr → m ν
        , bind ∷ (Variable, l) → m ν → m ν
        , substitute ∷ ρ → (Variable, l) → m ν → m ν
        , storeₗ ∷ (l, ν) → m ()
        , alloc ∷ Variable → m l
        , op2 ∷ Op2 → m ν → m ν → m ν
        , if' ∷ m ν → m ν → m ν → m ν
        } →
        Interpreter ν ρ m

liftInterpreter ∷ ∀ ν ρ m t. (MonadExc (t m), MonadTrans t, Monad (t m)) ⇒ Interpreter ν ρ m → Interpreter ν ρ (t m)
liftInterpreter Interpreter{..} =
    Interpreter
        { lambda = ζ₀ ∘ lambda
        , number = ζ₀ ∘ number
        , text = ζ₀ ∘ text
        , load = ζ₀ ∘ load
        , closure = \ν → ζ₀ ∘ closure ν
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
