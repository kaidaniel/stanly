{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Stanly.Fmt where

-- no-haskell-unicode
import Data.Coerce
import Stanly.Unicode

newtype ANSI = ANSI [(ESC, String)] deriving (Eq, Show, Semigroup, Monoid)

newtype ESC = ESC String deriving (Eq, Show, Semigroup, Monoid)

(⊹), (|-|) ∷ (Fmt a, Fmt b) ⇒ a → b → ANSI
(|-|) a b = fmt a ⋄ fmt b
(⊹) = (|-|)
infixr 6 |-|, ⊹
{-# INLINE (⊹) #-}

ansi ∷ Int → ESC
ansi n = ESC ("\x1b[" ⋄ show n ⋄ "m")

reset, bold, dim, italic, underline, black, red, green, yellow, blue, magenta, cyan, white, dflt ∷ ESC
reset = ansi 0
bold = ansi 1
dim = ansi 2
italic = ansi 3
underline = ansi 4
black = ansi 90
red = ansi 31
green = ansi 32
yellow = ansi 33
blue = ansi 34
magenta = ansi 35
cyan = ansi 36
white = ansi 37
dflt = ansi 39

display ∷ String → ANSI
display = ANSI ∘ ω ∘ (ESC "",)

bwText, ttyText ∷ (Fmt a) ⇒ a → String
bwText = bwText' ∘ fmt
ttyText = ttyText' ∘ fmt

bwText', ttyText' ∷ ANSI → String
bwText' (ANSI xs) = κλ π₂ xs
ttyText' (ANSI xs) = κλ (coerce \(cmd, str) → cmd <> str <> reset) xs

class Fmt a where fmt ∷ a → ANSI
instance Fmt Integer where fmt = display ∘ show
instance Fmt Int where fmt = display ∘ show
instance Fmt ANSI where fmt = id
instance Fmt Char where fmt = display ∘ ω
instance Fmt String where fmt = display
instance Fmt [ANSI] where fmt = κ₁
instance Fmt [String] where fmt = κ₁ ∘ φ display
instance Fmt ESC where fmt = ANSI ∘ ω ∘ (,"")
