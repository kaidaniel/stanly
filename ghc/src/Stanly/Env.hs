module Stanly.Env (Env (..), Val (..), pruneᵥ, regionᵣ, regionᵥ) where

import Stanly.Fmt (Fmt (..), FmtCmd (Bold, Dim, Yellow), (⊹))
import Stanly.Language (Expr, Variable, freeVars)
import Stanly.Unicode

newtype Env l where
    Env ∷ [(Variable, l)] → Env l
    deriving (Eq, Foldable, Semigroup, Monoid)

data Val l where
    LamV ∷ (Fmt l) ⇒ Variable → Expr → Env l → Val l
    NumV ∷ Integer → Val l
    TxtV ∷ String → Val l

deriving instance (Eq l) ⇒ Eq (Val l)
deriving instance Foldable Val

pruneᵥ ∷ Val l → Val l
pruneᵥ = \case
    LamV x e (Env r) → LamV x e ⎴ Env [(var, l) | (var, l) ← r, var ∈ freeVars e]
    x → x

regionᵣ ∷ Env l → [l]
regionᵣ (Env r) = map π₂ r

regionᵥ ∷ Val l → [l]
regionᵥ = \case LamV _ _ r → regionᵣ r; _ → []

instance Show (Val l) where
    show = \case
        LamV x body _ → "LamV" ⋄ " " ⋄ show x ⋄ " " ⋄ show body
        NumV n → "NumV" ⋄ " " ⋄ show n
        TxtV s → "TxtV" ⋄ " " ⋄ show s

instance (Fmt l) ⇒ Fmt (Env l) where
    fmt (Env r) = (Yellow ⊹ "Γ⟦") ⊹ fmt₁ (r, "") ⊹ (Yellow ⊹ "⟧")
      where
        fmt₁ = \case
            ((v, a) : r₁, sep) → sep ⊹ v ⊹ ": " ⊹ (Yellow ⊹ a) ⊹ fmt₁ (r₁, ", ")
            ([], _) → ε₁

instance (Fmt l) ⇒ Fmt (Val l) where
    fmt = \case
        LamV x body r → "λ" ⊹ (Bold ⊹ x) ⊹ "." ⊹ body ⊹ " " ⊹ r
        NumV n → Dim ⊹ n
        TxtV s → Dim ⊹ s
