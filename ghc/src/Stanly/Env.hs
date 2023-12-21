{-# LANGUAGE BlockArguments #-}

module Stanly.Env (EnvT, runEnvT, Env, regionᵣ, lookupₗ, bind', pruneᵣ) where

import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Reader (MonadReader (ask), ReaderT, runReaderT)
import Data.Coerce (coerce)
import Stanly.Fmt (Fmt (..), FmtCmd (Yellow), bwText, (⊹))
import Stanly.Language (Variable)
import Stanly.Unicode

newtype Env l where
    Env ∷ [(Variable, l)] → Env l
    deriving (Eq, Ord)

type EnvT l m = ReaderT (Env l) m

runEnvT ∷ EnvT l m a → m a
runEnvT = flip runReaderT (Env [])

lookupₗ ∷ (Fmt l, Eq l, MonadError String m, MonadReader (Env l) m) ⇒ Variable → m l
lookupₗ var = ask ⇉ \(Env ρ) → case lookup var ρ of Just l → ω l; Nothing → throwError (show var ⋄ " not found in environment: " ⋄ bwText (Env ρ))

regionᵣ ∷ Env l → [l]
regionᵣ (Env r) = map π₂ r

bind' ∷ (Variable, l) → Env l → Env l
bind' (var, l) = coerce \r → (var, l) : r

pruneᵣ ∷ (Variable → Bool) → Env l → Env l
pruneᵣ predicate (Env r) = Env [(var, l) | (var, l) ← r, predicate var]

instance (Fmt l) ⇒ Fmt (Env l) where
    fmt (Env r) = (Yellow ⊹ "Γ⟦") ⊹ fmt₁ (r, "") ⊹ (Yellow ⊹ "⟧")
      where
        fmt₁ = \case
            ((v, a) : r₁, sep) → sep ⊹ v ⊹ ": " ⊹ (Yellow ⊹ a) ⊹ fmt₁ (r₁, ", ")
            ([], _) → ε₁
