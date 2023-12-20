module Stanly.MachineState (Env (..), Store (..), Val (..)) where

import Data.Coerce (coerce)
import Stanly.Fmt (Fmt (..), FmtCmd (Bold, Dim, Yellow), bwText, (⊹))
import Stanly.Language (Expr, Var)
import Stanly.Unicode

newtype Env l where
    Env ∷ [(Var, l)] → Env l
    deriving (Eq, Foldable, Semigroup, Monoid)

newtype Store l where
    Store ∷ [(l, Val l)] → Store l
    deriving (Foldable, Semigroup, Monoid)

data Val l where
    LamV ∷ (Fmt l) ⇒ Var → Expr → Env l → Val l
    NumV ∷ Integer → Val l
    TxtV ∷ String → Val l

deriving instance (Eq l) ⇒ Eq (Val l)
deriving instance Foldable Val

instance (Fmt l) ⇒ Fmt (Env l) where
    fmt (Env r) = (Yellow ⊹ "Γ⟦") ⊹ fmt₁ (r, "") ⊹ (Yellow ⊹ "⟧")
      where
        fmt₁ = \case
            ((v, a) : r₁, sep) → sep ⊹ v ⊹ ": " ⊹ (Yellow ⊹ a) ⊹ fmt₁ (r₁, ", ")
            ([], _) → "" ⊹ ""

instance (Fmt l) ⇒ Fmt (Store l) where
    fmt =
        coerce @_ @[(l, Val l)] ⋙ reverse ⋙ \case
            [] → ε₁
            (x : xs) → line x ⊹ ["\n" ⊹ line x₁ | x₁ ← xs]
      where
        prefix = (Dim ⊹) ∘ \case LamV{} → "lam "; NumV{} → "num "; TxtV{} → "txt "
        line (k, v) = (Dim ⊹ "stor ") ⊹ (Yellow ⊹ (padded ⎴ bwText k) ⊹ " ") ⊹ prefix v ⊹ v
          where
            padded ∷ String → String
            padded = \case
                [] → "    "
                s@[_] → "   " ⋄ s
                s@[_, _] → "  " ⋄ s
                s@[_, _, _] → " " ⋄ s
                s → s

instance (Fmt l) ⇒ Fmt (Val l) where
    fmt = \case
        LamV x body r → "λ" ⊹ (Bold ⊹ x) ⊹ "." ⊹ body ⊹ " " ⊹ r
        NumV n → Dim ⊹ n
        TxtV s → Dim ⊹ s
