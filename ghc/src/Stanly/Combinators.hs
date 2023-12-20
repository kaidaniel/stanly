{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}

module Stanly.Combinators (ev, evTrace, evDeadCode) where

import Control.Monad.Writer.Strict (MonadWriter (tell), censor)
import Data.Char (toLower)
import Data.Coerce (coerce)
import Data.List qualified as L
import Stanly.Fmt (Fmt (..), FmtCmd (Dim), (⊹))
import Stanly.Interpreter qualified as I
import Stanly.Language qualified as L
import Stanly.MachineState qualified as I
import Stanly.Unicode

ev ∷ ∀ l m. (Monad m) ⇒ I.Interpreter l m → I.Eval l m
ev = I.makeInterpreter id id

evTrace ∷ ∀ l m. (MonadWriter (ProgramTrace l) m) ⇒ I.Interpreter l m → I.Eval l m
evTrace i@I.Interpreter{..} = I.makeInterpreter id open i
  where
    open evalTr eval expr = do
        ρ ← env
        σ ← store
        tell ⎴ coerce [(expr, ρ, σ)]
        evalTr eval expr

evDeadCode ∷ ∀ m l. (MonadWriter NotCovered m) ⇒ I.Interpreter l m → I.Eval l m
evDeadCode = I.makeInterpreter closed open
  where
    closed eval expr = censor (coerce \used → cleanUp ⎴ L.subexprs expr L.\\ used) ⎴ eval expr
    cleanUp x = reverse ⎴ x L.\\ (x ⇉ L.subexprs)
    open evalTr eval expr = do tell ⎴ NotCovered [expr]; evalTr eval expr

newtype ProgramTrace l = ProgramTrace [(L.Expr, I.Env l, I.Store l)] deriving (Semigroup, Monoid, Foldable)

instance (Fmt l) ⇒ Fmt (ProgramTrace l) where
    fmt (ProgramTrace li) = κ₁ [Dim ⊹ i ⊹ expr₁ e ⊹ env₁ r ⊹ store₁ s ⊹ "\n" | ((e, r, s), i) ← zip li [1 ∷ Integer ..]]
      where
        expr₁ e = Dim ⊹ "\n" ⊹ [toLower x | x ← take 3 ⎴ show e] ⊹ " " ⊹ e
        env₁ r = Dim ⊹ "\nenvr " ⊹ r
        store₁ = \case I.Store [] → fmt ""; x → "\n" ⊹ x

newtype NotCovered = NotCovered [L.Expr] deriving (Eq, Show, Semigroup, Monoid)

instance Fmt NotCovered where fmt = coerce ⎴ \case [] → ε₁; [x] → fmt x; (x : xs) → x ⊹ "\n" ⊹ NotCovered xs
