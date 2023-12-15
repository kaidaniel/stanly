module Stanly.Combinators (ev, evTrace, evDeadCode) where

import Control.Monad.Writer.Strict
import Data.Char qualified as C
import Data.Coerce (coerce)
import Data.List qualified as L
import Stanly.Fmt
import Stanly.Interpreter qualified as I
import Stanly.Unicode

ev ∷ ∀ l m. (Monad m) ⇒ I.Combinator l m
ev i = I.interpret i id id

evTrace ∷ ∀ l m. (MonadWriter (ProgramTrace l) m) ⇒ I.Combinator l m
evTrace i = I.interpret i id open
  where
    open evalTr eval expr = do
        r ← I.env i
        s ← I.store i
        tell ⎴ coerce [(expr, r, s)]
        evalTr eval expr

evDeadCode ∷ ∀ m l. (MonadWriter NotCovered m) ⇒ I.Combinator l m
evDeadCode i = I.interpret i closed open
  where
    closed eval expr = censor (coerce (\used → cleanUp ([expr] ⋄ I.subexprs expr L.\\ used))) (eval expr)
    cleanUp dead = reverse (dead L.\\ (dead ⇉ I.subexprs))
    open evalTr eval expr = do
        tell ⎴ coerce [expr]
        evalTr eval expr

newtype ProgramTrace l = ProgramTrace [(I.Expr, I.Env l, I.Store l)] deriving (Semigroup, Monoid, Foldable)

instance (Fmt l) ⇒ Fmt (ProgramTrace l) where
    ansiFmt (ProgramTrace li) = mconcat [dim ⊹ i ⊹ expr' e ⊹ env' r ⊹ store' s ⊹ "\n" | ((e, r, s), i) ← zip li [1 ∷ Integer ..]]
      where
        expr' e = dim ⊹ "\n" ⊹ [C.toLower x | x ← take 3 ⎴ show e] ⊹ " " ⊹ e
        env' r = dim ⊹ "\nenvr " ⊹ r
        store' = \case I.Store [] → ansiFmt ""; x → "\n" ⊹ x

newtype NotCovered = NotCovered [I.Expr] deriving (Eq, Show, Semigroup, Monoid)

instance Fmt NotCovered where
    ansiFmt =
        coerce ⎴ \case
            [] → ε₁
            [x] → ansiFmt x
            (x : xs) → x ⊹ "\n" ⊹ NotCovered xs