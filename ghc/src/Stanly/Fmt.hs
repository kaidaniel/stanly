{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Stanly.Fmt where

-- no-haskell-unicode
import Data.Coerce
import Stanly.Unicode

newtype ANSI = ANSI {unANSI ∷ [(ESC, String)]} deriving (Eq, Show, Semigroup, Monoid)

newtype ESC = ESC {unESC ∷ String} deriving (Eq, Show, Semigroup, Monoid)

(⊹), (|-|) ∷ (Fmt a, Fmt b) ⇒ a → b → ANSI
(|-|) a b = ansiFmt a ⋄ ansiFmt b
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
display s = ANSI [(ESC "", s)]

class Fmt a where
    fmt ∷ a → String
    fmt e = mconcat [snd x | let ANSI xs = ansiFmt e, x ← xs]
    termFmt ∷ a → String
    termFmt e = mconcat [coerce code ⋄ t ⋄ coerce reset | let ANSI xs = ansiFmt e, (code, t) ← xs]
    ansiFmt ∷ a → ANSI

instance Fmt Integer where
    ansiFmt i = display $ show i

instance Fmt Int where ansiFmt i = display $ show i

instance Fmt ANSI where
    ansiFmt = id

instance Fmt Char where
    ansiFmt c = display [c]

instance Fmt String where
    ansiFmt = display

instance Fmt [ANSI] where
    ansiFmt = mconcat

instance Fmt [String] where
    ansiFmt = mconcat . fmap display

instance Fmt ESC where
    ansiFmt esc = ANSI [(esc, "")]
