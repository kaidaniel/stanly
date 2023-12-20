{-# LANGUAGE BlockArguments #-}

module Stanly.Combinators (ev, evTrace, evDeadCode) where

import Control.Monad.Reader (MonadReader (ask))
import Control.Monad.State (MonadState (get))
import Control.Monad.Writer (MonadWriter (tell), censor)
import Data.Char (toLower)
import Data.Coerce (coerce)
import Data.List ((\\))
import Stanly.Fmt (Fmt (..), FmtCmd (Dim), (⊹))
import Stanly.Interpreter (Eval, Interpreter (..), makeInterpreter)
import Stanly.Language (Expr, subexprs)
import Stanly.MachineState (Env, Store (..))
import Stanly.Unicode

ev ∷ ∀ l m. (Monad m) ⇒ Interpreter l m → Eval l m
ev = makeInterpreter id id

evTrace ∷ ∀ l m. (MonadWriter (ProgramTrace l) m, MonadState (Store l) m, MonadReader (Env l) m) ⇒ Interpreter l m → Eval l m
evTrace = makeInterpreter id open
  where
    open evalTr eval expr = do
        ρ ← ask
        σ ← get
        tell ⎴ coerce [(expr, ρ, σ)]
        evalTr eval expr

evDeadCode ∷ ∀ m l. (MonadWriter NotCovered m) ⇒ Interpreter l m → Eval l m
evDeadCode = makeInterpreter closed open
  where
    closed eval expr = censor (coerce \used → cleanUp ⎴ subexprs expr \\ used) ⎴ eval expr
    cleanUp x = reverse ⎴ x \\ (x ⇉ subexprs)
    open evalTr eval expr = do tell ⎴ NotCovered [expr]; evalTr eval expr

newtype ProgramTrace l = ProgramTrace [(Expr, Env l, Store l)] deriving (Semigroup, Monoid, Foldable)

instance (Fmt l) ⇒ Fmt (ProgramTrace l) where
    fmt (ProgramTrace li) = κ₁ [Dim ⊹ i ⊹ expr₁ e ⊹ env₁ ρ ⊹ store₁ σ ⊹ "\n" | ((e, ρ, σ), i) ← zip li [1 ∷ Integer ..]]
      where
        expr₁ e = Dim ⊹ "\n" ⊹ [toLower x | x ← take 3 ⎴ show e] ⊹ " " ⊹ e
        env₁ ρ = Dim ⊹ "\nenvr " ⊹ ρ
        store₁ = \case Store [] → ε₁; x → "\n" ⊹ x

newtype NotCovered = NotCovered [Expr] deriving (Eq, Show, Semigroup, Monoid)

instance Fmt NotCovered where fmt = coerce ⎴ \case [] → ε₁; [x] → fmt x; (x : xs) → x ⊹ "\n" ⊹ NotCovered xs
