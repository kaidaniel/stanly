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
        tell $ ProgramTrace [(expr, r, s)]
        evalTr eval expr

evDeadCode ∷ ∀ m l. (MonadWriter NotCovered m) ⇒ I.Combinator l m
evDeadCode i = I.interpret i closed open
  where
    closed eval expr = censor (coerce (\used → cleanUp ([expr] ⋄ I.subexprs expr L.\\ used))) (eval expr)
    cleanUp dead = reverse (dead L.\\ (dead >>= I.subexprs))
    open evalTr eval expr = do
        tell $ NotCovered [expr]
        evalTr eval expr

newtype ProgramTrace l = ProgramTrace [(I.Expr, I.Env l, I.Store l)] deriving (Show, Semigroup, Monoid, Foldable)

instance (Show l) ⇒ Fmt (ProgramTrace l) where
    ansiFmt ∷ ProgramTrace l → ANSI
    ansiFmt (ProgramTrace li) = join' (zip (map f li) [1 ..])
      where
        join' ∷ [(ANSI, Integer)] → ANSI
        join' [] = ε₁
        join' [(a, i)] = dim >+ show i ⋄ a ⋄ start "\n"
        join' (x : xs) = join' [x] ⋄ join' xs

        f ∷ (I.Expr, I.Env l, I.Store l) → ANSI
        f (e, r, s) = dim >+ ("\n" ⋄ name e ⋄ " ") ⋄ ansiFmt e ⋄ dim >+ "\nenvr " ⋄ ansiFmt r ⋄ g s
        g (I.Store []) = start ""
        g x = start "\n" ⋄ ansiFmt x
        name = map C.toLower . L.take 3 . show

newtype NotCovered = NotCovered [I.Expr] deriving (Eq, Show, Semigroup, Monoid)

instance Fmt NotCovered where
    ansiFmt (NotCovered li) = case li of
        [] → ε₁
        [x] → ansiFmt x
        (x : xs) → ansiFmt x ⋄ start "\n" ⋄ ansiFmt (NotCovered xs)
