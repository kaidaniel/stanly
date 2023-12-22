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

idₘ ∷ ∀ l m. (Monad m) ⇒ Interpreter l m → Eval l m
idₘ = makeInterpreter id id

newtype ProgramTrace r s = ProgramTrace [(Expr, r, s)] deriving (Semigroup, Monoid)

trace ∷ ∀ l m r s. (MonadWriter (ProgramTrace r s) m, MonadState s m, MonadReader r m) ⇒ Interpreter l m → Eval l m
trace = makeInterpreter id open
  where
    open evalTr eval expr = do
        ρ ← ask
        σ ← get
        tell ⎴ ProgramTrace [(expr, ρ, σ)]
        evalTr eval expr

newtype NotCovered = NotCovered [Expr] deriving (Semigroup, Monoid)

dead ∷ ∀ m l. (MonadWriter NotCovered m) ⇒ Interpreter l m → Eval l m
dead = makeInterpreter closed open
  where
    closed eval expr = censor (\(NotCovered used) → NotCovered (cleanUp ⎴ subexprs expr \\ used)) ⎴ eval expr
    cleanUp x = reverse ⎴ x \\ (x ⇉ subexprs)
    open evalTr eval expr = do tell ⎴ NotCovered [expr]; evalTr eval expr

instance (Fmt s, Fmt r) ⇒ Fmt (ProgramTrace r s) where
    fmt (ProgramTrace li) = κ₁ [Dim ⊹ i ⊹ expr₁ e ⊹ (Dim ⊹\ "envr ") ⊹ ρ ⊹\ σ ⊹ "\n" | ((e, ρ, σ), i) ← zip li [1 ∷ Integer ..]]
      where
        expr₁ e = Dim ⊹\ [toLower x | x ← take 3 ⎴ show e] ⊹ " " ⊹ e

instance Fmt NotCovered where fmt (NotCovered li) = case li of [] → ε₁; [x] → fmt x; (x : xs) → x ⊹\ NotCovered xs
