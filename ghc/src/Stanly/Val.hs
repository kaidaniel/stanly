module Stanly.Val (Val, pruneᵥ, regionᵥ, closureᵥ, numberᵥ, textᵥ, lambda, arithmetic, ifn0) where

import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Reader (MonadReader (ask))
import Stanly.Env (Env, pruneᵣ, regionᵣ)
import Stanly.Fmt (Fmt (..), FmtCmd (Bold, Dim), bwText, (⊹))
import Stanly.Language (Expr, Op2 (..), Variable, freeVars)
import Stanly.Unicode

data Val l where
    LamV ∷ (Fmt l) ⇒ Variable → Expr → Env l → Val l
    NumV ∷ Integer → Val l
    TxtV ∷ String → Val l

regionᵥ ∷ Val l → [l]
regionᵥ = \case
    LamV _ _ r → regionᵣ r
    _ → []

pruneᵥ ∷ Val l → Val l
pruneᵥ = \case
    LamV x e r → LamV x e (pruneᵣ (∈ freeVars e) r)
    x → x

numberᵥ ∷ (Monad m) ⇒ Integer → m (Val l)
numberᵥ = ω ∘ NumV

textᵥ ∷ (Monad m) ⇒ String → m (Val l)
textᵥ = ω ∘ TxtV

closureᵥ ∷ (Fmt l, MonadReader (Env l) m) ⇒ Variable → Expr → m (Val l)
closureᵥ x e = φ (LamV x e) ask

exception ∷ (MonadError String m) ⇒ String → m a
exception msg = throwError ⎴ "Exception: " ⋄ msg

lambda ∷ (Fmt l, MonadError String m) ⇒ Val l → m (Variable, Expr, Env l)
lambda val = case val of
    LamV x e r → ω (x, e, r)
    _ → exception ⎴ "'" ⋄ bwText val ⋄ "'" ⋄ " is not a function."

arithmetic ∷ (Fmt l, MonadError String m) ⇒ Op2 → Val l → Val l → m (Val l)
arithmetic o a b = do
    let exc msg = exception ⎴ bwText ⎴ msg ⊹ ".\nWhen evaluating: " ⊹ a ⊹ o ⊹ b
    case (a, b) of
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

ifn0 ∷ (MonadError String m) ⇒ Val l → m (Val l) → m (Val l) → m (Val l)
ifn0 tst then' else' = case tst of
    NumV n | n == 0 → else' | otherwise → then'
    _ → exception "Branching on non-numeric value."

instance Show (Val l) where
    show = \case
        LamV x body _ → "LamV" ⋄ " " ⋄ show x ⋄ " " ⋄ show body
        NumV n → "NumV" ⋄ " " ⋄ show n
        TxtV s → "TxtV" ⋄ " " ⋄ show s

instance (Fmt l) ⇒ Fmt (Val l) where
    fmt = \case
        LamV x body r → "λ" ⊹ (Bold ⊹ x) ⊹ "." ⊹ body ⊹ " " ⊹ r
        NumV n → Dim ⊹ n
        TxtV s → Dim ⊹ s
