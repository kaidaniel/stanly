{-# LANGUAGE BlockArguments #-}

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
trace = makeInterpreter id open
  where
    open evalTr eval expr = do
        ρ ← ask
        σ ← get
        tell ⎴ Trace [(expr, ρ, σ)]
        evalTr eval expr

newtype Dead = Dead [Expr] deriving (Semigroup, Monoid)

dead ∷ ∀ l ν ρ m. (MonadWriter Dead m) ⇒ Interpreter l ν ρ m → Eval m ν
dead = makeInterpreter closed open
  where
    closed eval expr = censor (\(Dead used) → Dead (cleanUp ⎴ subexprs expr \\ used)) ⎴ eval expr
    cleanUp x = reverse ⎴ x \\ (x ⇉ subexprs)
    open evalTr eval expr = do tell ⎴ Dead [expr]; evalTr eval expr

instance (Fmt ρ, Fmt σ) ⇒ Fmt (Trace ρ σ) where
    fmt (Trace li) = κ₁ (φ ln (zip li [1 ∷ Integer ..]))
      where
        low3 e = [toLower x | x ← take 3 ⎴ show e] <> "  "
        ln ((e, ρ, σ), i) = (Dim ⊹ i ⊹\ low3 e) ⊹ " " ⊹ (e ⊹\ ((Dim ⊹ "envir ") ⊹ ρ) ⊹\ σ) ⊹ "\n"

instance Fmt Dead where
    fmt (Dead li) = case li of [] → ε₁; [x] → fmt x; (x : xs) → x ⊹\ Dead xs
