{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}

module Stanly.Interpreter (Interpreter (..), makeInterpreter, arithmetic, branchIfn0, Eval) where

import Control.Monad.Except (MonadError, fix, throwError)
import GHC.Base (coerce)
import Stanly.Fmt (Fmt, bwText, (⊹))
import Stanly.Language (Expr (..), Op2 (..), Variable)
import Stanly.MachineState (Env (..), Store (..), Val (..))
import Stanly.Unicode

type Eval l m = Expr → m (Val l)

makeInterpreter ∷ ∀ l m. (Eval l m → Eval l m) → ((Eval l m → Eval l m) → (Eval l m → Eval l m)) → Interpreter l m → Eval l m
makeInterpreter closed open interpreter = closed ⎴ fix ⎴ open ⎴ eval interpreter

eval ∷ ∀ m l. Interpreter l m → Eval l m → Eval l m
eval Interpreter{..} eval₁ = \case
    Num n → ω (NumV n)
    Txt s → ω (TxtV s)
    Lam v e → φ (LamV v e) env
    Var var → search var
    If tst then' else' → branch (eval₁ tst) (eval₁ then') (eval₁ else')
    Op2 o e₁ e₂ → op2 o (eval₁ e₁) (eval₁ e₂)
    Rec var body → do
        loc ← alloc var
        value ← localEnv₂ (var, loc) body
        updateStore₁ (loc, value)
        ω value
    App f x →
        eval₁ f ⇉ \case
            LamV var body ρ₁ → do
                x₁ ← eval₁ x
                loc ← alloc var
                updateStore₁ (loc, x₁)
                localEnv₁ ρ₁ (var, loc) body
            _ → exc ⎴ notAFunction f x
  where
    notAFunction f x =
        "Left hand side of application not bound to a function."
            ⋄ "\n\nIn function position ⋙ "
            ⋄ bwText f
            ⋄ "\nIn argument position ⋙ "
            ⋄ bwText x
    search var =
        env ⇉ \ρ → case lookup var (coerce ρ) of
            Just l → deref l
            Nothing → exc (show var ⋄ " not found in environment: " ⋄ bwText ρ)
    localEnv₂ binding cc = localEnv (coerce [binding] ⋄) ⎴ eval₁ cc
    localEnv₁ ρ₁ binding cc = localEnv (const (coerce [binding] ⋄ ρ₁)) ⎴ eval₁ cc
    updateStore₁ binding = updateStore (coerce [binding] ⋄)

data Interpreter l m where
    Interpreter ∷
        (Fmt l, Monad m) ⇒
        { deref ∷ l → m (Val l)
        , env ∷ m (Env l)
        , localEnv ∷ (Env l → Env l) → m (Val l) → m (Val l)
        , store ∷ m (Store l)
        , updateStore ∷ (Store l → Store l) → m ()
        , alloc ∷ Variable → m l
        , op2 ∷ Op2 → m (Val l) → m (Val l) → m (Val l)
        , branch ∷ m (Val l) → m (Val l) → m (Val l) → m (Val l)
        , exc ∷ String → m (Val l)
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
