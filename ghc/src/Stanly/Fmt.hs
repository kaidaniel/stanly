{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Stanly.Fmt where

import Data.Text qualified as T

newtype ANSI = ANSI {unANSI ∷ [(ESC, T.Text)]} deriving (Eq, Show, Semigroup, Monoid)

newtype ESC = ESC {unESC ∷ T.Text} deriving (Eq, Show, Semigroup, Monoid)

(>+) ∷ ESC → String → ANSI
(>+) esc rhs = ANSI [(esc, T.pack rhs)]

start ∷ String → ANSI
start s = ANSI [(ESC "", T.pack s)]

ansi ∷ Int → ESC
ansi n = ESC ("\x1b[" <> T.pack (show n) <> "m")

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

class Fmt a where
    fmt ∷ a → String
    fmt e = let ANSI x = ansiFmt e in T.unpack (foldl (\txt (_, t) → txt <> t) mempty x)
    termFmt ∷ a → String
    termFmt e = let ANSI x = ansiFmt e in T.unpack (foldl (\txt (ESC code, t) → ((txt <> code) <> t) <> unESC reset) mempty x)
    ansiFmt ∷ a → ANSI

instance (Fmt a, Fmt b) ⇒ Fmt (a, b) where
    ansiFmt (a, b) = start "(" <> ansiFmt a <> start ", " <> ansiFmt b <> start ")"

instance (Fmt a, Fmt b, Fmt c) ⇒ Fmt (a, b, c) where
    ansiFmt (a, b, c) = dim >+ "(" <> ansiFmt a <> start ", " <> ansiFmt b <> start ", " <> ansiFmt c <> dim >+ ")"
