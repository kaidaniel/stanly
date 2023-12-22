module Stanly.Val (Val, prune, regionᵥ, closureᵥ, number, text, lambda, arithmetic, ifn0) where

import Control.Monad.Reader (MonadReader (ask))
import Stanly.Env (Env, pruneEnv, regionᵣ)
import Stanly.Exc (MonadExc, exc)
import Stanly.Fmt (Fmt (..), FmtCmd (Bold, Dim), (⊹))
import Stanly.Language (Expr, Op2 (..), Variable, freeVars)
import Stanly.Unicode

data Val l where
    LamV ∷ (Fmt l) ⇒ Variable → Expr → Env l → Val l
    NumV ∷ Integer → Val l
    TxtV ∷ String → Val l

deriving instance (Eq l) ⇒ Eq (Val l)
deriving instance (Ord l) ⇒ Ord (Val l)

regionᵥ ∷ Val l → [l]
regionᵥ = \case
    LamV _ _ r → regionᵣ r
    _ → []

prune ∷ Val l → Val l
prune = \case
    LamV x e r → LamV x e (pruneEnv (∈ freeVars e) r)
    x → x

number ∷ (Monad m) ⇒ Integer → m (Val l)
number = ω ∘ NumV

text ∷ (Monad m) ⇒ String → m (Val l)
text = ω ∘ TxtV

closureᵥ ∷ (Fmt l, MonadReader (Env l) m) ⇒ Variable → Expr → m (Val l)
closureᵥ x e = φ (LamV x e) ask

lambda ∷ (Fmt l, MonadExc m) ⇒ Val l → m (Variable, Expr, Env l)
lambda val = case val of
    LamV x e r → ω (x, e, r)
    _ → exc ⎴ "'" ⊹ val ⊹ "'" ⊹ " is not a function."

arithmetic ∷ (Fmt l, MonadExc m) ⇒ Op2 → Val l → Val l → m (Val l)
arithmetic o a b = do
    let exc₁ msg = exc ⎴ msg ⊹ ".\nWhen evaluating: " ⊹ a ⊹ o ⊹ b
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
        _ → exc₁ "Invalid arguments to operator"

flattenedArithmetic ∷ (Fmt l, MonadExc m) ⇒ Op2 → Val l → Val l → m (Val l)
flattenedArithmetic o a b = do
    undefined

ifn0 ∷ (MonadExc m) ⇒ Val l → m (Val l) → m (Val l) → m (Val l)
ifn0 tst then' else' = case tst of
    NumV n | n == 0 → else' | otherwise → then'
    _ → exc "Branching on non-numeric value."

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
