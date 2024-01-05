module Stanly.Mixins (idₘ, trace, dead) where

import Control.Monad.Reader (MonadReader (ask))
import Control.Monad.State (MonadState (get))
import Control.Monad.Writer (MonadWriter (tell), censor)
import Data.Char (toLower)
import Data.List ((\\))
import Stanly.Fmt (Fmt (..), FmtCmd (Dim), (⊹), (⊹\))
import Stanly.Interpreter (Eval, Interpreter (..), makeInterpreter)
import Stanly.Language (Expr, subexprs)
import Stanly.Unicode

idₘ ∷ ∀ l ν ρ m. (Monad m) ⇒ Interpreter l ν ρ m → Eval m ν
idₘ = makeInterpreter id id

newtype Trace ρ σ = Trace [(Expr, ρ, σ)] deriving (Semigroup, Monoid)

trace ∷
    ∀ l ν ρ σ m.
    (MonadWriter (Trace ρ σ) m, MonadState σ m, MonadReader ρ m) ⇒
    Interpreter l ν ρ m →
    Eval m ν
trace = makeInterpreter
    id
    \evTr ev e → do
        tell ⇇ φ (\ρ σ → γ [(e, ρ, σ)]) ask ⊛ get
        evTr ev e

newtype Dead = Dead [Expr] deriving (Semigroup, Monoid)

dead ∷ ∀ l ν ρ m. (MonadWriter Dead m) ⇒ Interpreter l ν ρ m → Eval m ν
dead = makeInterpreter
    (\ev e → censor (γ ⎴ reverse ∘ (\es → es \\ (es ⇉ subexprs)) ∘ (subexprs e \\)) ⎴ ev e)
    \evTr ev e → do
        tell ⎴ γ [e]
        evTr ev e

instance (Fmt ρ, Fmt σ) ⇒ Fmt (Trace ρ σ) where
    fmt (Trace li) = κ₁ (φ ln (zip li [1 ∷ Integer ..]))
      where
        low3 e = [toLower x | x ← take 3 ⎴ show e] <> "  "
        ln ((e, ρ, σ), i) = (Dim ⊹ i ⊹\ low3 e) ⊹ " " ⊹ (e ⊹\ ((Dim ⊹ "envir ") ⊹ ρ) ⊹\ σ) ⊹ "\n"

instance Fmt Dead where
    fmt (Dead li) = case li of [] → ε₁; [x] → fmt x; (x : xs) → x ⊹\ Dead xs
