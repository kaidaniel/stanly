module Stanly.Interpreter (Interpreter (..), makeInterpreter, Eval) where

import Data.Function (fix)
import Stanly.Fmt (Fmt)
import Stanly.Language (Expr (..), Op2, Variable)
import Stanly.Unicode

type Eval m a = Expr → m a

makeInterpreter ∷
    ∀ l ν ρ m.
    (Eval m ν → Eval m ν) →
    ((Eval m ν → Eval m ν) → (Eval m ν → Eval m ν)) →
    Interpreter l ν ρ m →
    Eval m ν
makeInterpreter closed open interpreter = closed ⎴ fix ⎴ open ⎴ eval interpreter
  where
    eval ∷ Interpreter l ν ρ m → Eval m ν → Eval m ν
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

data Interpreter loc val env m where
    Interpreter ∷
        (Fmt loc, Monad m) ⇒
        { lambda ∷ val → m (Variable, Expr, env)
        , number ∷ Integer → m val
        , text ∷ String → m val
        , closure ∷ Variable → Expr → m val
        , load ∷ Variable → m val
        , bind ∷ (Variable, loc) → m val → m val
        , substitute ∷ env → (Variable, loc) → m val → m val
        , storeₗ ∷ (loc, val) → m ()
        , alloc ∷ Variable → m loc
        , op2 ∷ Op2 → m val → m val → m val
        , if' ∷ m val → m val → m val → m val
        } →
        Interpreter loc val env m
