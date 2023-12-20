{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}

module Stanly.Interpreter (Interpreter (..), makeInterpreter, arithmetic, branchIfn0, Eval) where

import Control.Monad.Except (MonadError, fix, throwError)
import Stanly.Env (Env (..), Val (..))
import Stanly.Fmt (Fmt, bwText, (⊹))
import Stanly.Language (Expr (..), Op2 (..), Variable)
import Stanly.Unicode

type Eval l m = Expr → m (Val l)

makeInterpreter ∷ ∀ l m. (Eval l m → Eval l m) → ((Eval l m → Eval l m) → (Eval l m → Eval l m)) → Interpreter l m → Eval l m
makeInterpreter closed open interpreter = closed ⎴ fix ⎴ open ⎴ eval interpreter

eval ∷ ∀ m l. Interpreter l m → Eval l m → Eval l m
eval Interpreter{..} eval₁ = \case
    Num n → ω (NumV n)
    Txt s → ω (TxtV s)
    Lam x e → closure (LamV x e)
    Var x → load x
    If tst then' else' → branch (eval₁ tst) (eval₁ then') (eval₁ else')
    Op2 o e₁ e₂ → op2 o (eval₁ e₁) (eval₁ e₂)
    Rec var body → do
        loc ← alloc var
        value ← bind (var, loc) (eval₁ body)
        storeₗ (loc, value)
        ω value
    App f x →
        eval₁ f ⇉ \case
            LamV var body ρ₁ → do
                x₁ ← eval₁ x
                loc ← alloc var
                storeₗ (loc, x₁)
                substitute ρ₁ (var, loc) (eval₁ body)
            _ → throwError ⎴ notAFunction f x

data Interpreter l m where
    Interpreter ∷
        (Fmt l, Monad m, MonadError String m) ⇒
        { load ∷ Variable → m (Val l)
        , closure ∷ (Env l → Val l) → m (Val l)
        , bind ∷ (Variable, l) → m (Val l) → m (Val l)
        , substitute ∷ Env l → (Variable, l) → m (Val l) → m (Val l)
        , storeₗ ∷ (l, Val l) → m ()
        , alloc ∷ Variable → m l
        , op2 ∷ Op2 → m (Val l) → m (Val l) → m (Val l)
        , branch ∷ m (Val l) → m (Val l) → m (Val l) → m (Val l)
        } →
        Interpreter l m

exception ∷ (MonadError String m) ⇒ String → m a
exception msg = throwError ⎴ "Exception: " ⋄ msg

arithmetic ∷ (Fmt l, MonadError String m) ⇒ Op2 → m (Val l) → m (Val l) → m (Val l)
arithmetic o a b = do
    a₁ ← a
    b₁ ← b
    let exc msg = exception ⎴ bwText ⎴ msg ⊹ ".\nWhen evaluating: " ⊹ a₁ ⊹ o ⊹ b₁
    case (a₁, b₁) of
        (NumV n₀, NumV n₁)
            | o == Plus → ω ⎴ NumV ⎴ n₀ + n₁
            | o == Minus → ω ⎴ NumV ⎴ n₀ - n₁
            | o == Times → ω ⎴ NumV ⎴ n₀ * n₁
            | o == Divide, n₁ == 0 → exc "Division by zero"
            | o == Divide → ω ⎴ NumV ⎴ div n₀ n₁
        (TxtV t₀, TxtV t₁)
            | o == Plus → ω ⎴ TxtV ⎴ t₀ ⋄ t₁
        (TxtV t₀, NumV n₁)
            | o == Plus → ω ⎴ TxtV ⎴ t₀ ⋄ show n₁
        _ → exc "Invalid arguments to operator"

branchIfn0 ∷ (MonadError String m) ⇒ m (Val l) → m (Val l) → m (Val l) → m (Val l)
branchIfn0 tst then' else' =
    tst ⇉ \case
        NumV n | n == 0 → else' | otherwise → then'
        _ → exception "Branching on non-numeric value."

notAFunction ∷ Expr → Expr → String
notAFunction f x =
    "Left hand side of application not bound to a function."
        ⋄ "\n\nIn function position ⋙ "
        ⋄ bwText f
        ⋄ "\nIn argument position ⋙ "
        ⋄ bwText x

-- liftInterpreter ∷ ∀ l m t. (MonadTrans t, Monad (t m)) ⇒ Interpreter l m → Interpreter l (t m)
-- liftInterpreter Interpreter{..} =
--     Interpreter
--         { deref = ζ₀ ∘ deref
--         , exc = ζ₀ ∘ exc
--         , env = ζ₀ env
--         , alloc = ζ₀ ∘ alloc
--         , localEnv = ζ₁ ∘ localEnv
--         , store = ζ₀ store
--         , updateStore = ζ₀ ∘ updateStore
--         , op2 = ζ₂ ∘ op2
--         , branch = ζ₃ branch
--         }
--   where
--     ζ₀ ∷ m r → t m r
--     ζ₁ ∷ (m x₁ → m r) → (t m x₁ → t m r)
--     ζ₂ ∷ (m x₁ → m x₂ → m r) → (t m x₁ → t m x₂ → t m r)
--     ζ₃ ∷ (m x₁ → m x₂ → m x₃ → m r) → (t m x₁ → t m x₂ → t m x₃ → t m r)

--     f ⊰ a = f ⊛ φ ω a
--     infixl 4 ⊰
--     ζ₀ = lift
--     ζ₁ f x = ω f ⊰ x ⇉ ζ₀
--     ζ₂ f x y = ω f ⊰ x ⊰ y ⇉ ζ₀
--     ζ₃ f x y z = ω f ⊰ x ⊰ y ⊰ z ⇉ ζ₀
