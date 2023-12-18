{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}

module Stanly.Combinators (ev, evTrace, evDeadCode) where

import Control.Monad.Writer.Strict
import Data.Char qualified as C
import Data.Coerce (coerce)
import Data.List qualified as L
import Stanly.Fmt
import Stanly.Interpreter qualified as I
import Stanly.Unicode

ev ∷ ∀ l m. (Monad m) ⇒ I.Combinator l m
ev = I.interpret id id

evTrace ∷ ∀ l m. (MonadWriter (ProgramTrace l) m) ⇒ I.Combinator l m
evTrace i@I.Interpreter{..} = I.interpret id open i
  where
    open evalTr eval expr = do
        r ← env
        s ← store
        tell ⎴ coerce [(expr, r, s)]
        evalTr eval expr

evDeadCode ∷ ∀ m l. (MonadWriter NotCovered m) ⇒ I.Combinator l m
evDeadCode = I.interpret closed open
  where
    closed eval expr = censor (coerce \used → cleanUp ⎴ I.subexprs expr L.\\ used) ⎴ eval expr
    cleanUp x = reverse ⎴ x L.\\ (x ⇉ I.subexprs)
    open evalTr eval expr = do tell ⎴ NotCovered [expr]; evalTr eval expr

newtype ProgramTrace l = ProgramTrace [(I.Expr, I.Env l, I.Store l)] deriving (Semigroup, Monoid, Foldable)

instance (Fmt l) ⇒ Fmt (ProgramTrace l) where
    fmt (ProgramTrace li) = κ₁ [dim ⊹ i ⊹ expr₁ e ⊹ env₁ r ⊹ store₁ s ⊹ "\n" | ((e, r, s), i) ← zip li [1 ∷ Integer ..]]
      where
        expr₁ e = dim ⊹ "\n" ⊹ [C.toLower x | x ← take 3 ⎴ show e] ⊹ " " ⊹ e
        env₁ r = dim ⊹ "\nenvr " ⊹ r
        store₁ = \case I.Store [] → fmt ""; x → "\n" ⊹ x

newtype NotCovered = NotCovered [I.Expr] deriving (Eq, Show, Semigroup, Monoid)

instance Fmt NotCovered where fmt = coerce ⎴ \case [] → ε₁; [x] → fmt x; (x : xs) → x ⊹ "\n" ⊹ NotCovered xs
