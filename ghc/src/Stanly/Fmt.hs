{-# LANGUAGE OverloadedStrings #-}

module Stanly.Fmt where

import Data.Text qualified as T
import Stanly.Eval (Env (..), Store (..))
import Stanly.Expr (Expr (..))

newtype ANSI = ANSI {unANSI :: [(ESC, T.Text)]} deriving (Eq, Show, Semigroup, Monoid)

newtype ESC = ESC {unESC :: T.Text} deriving (Eq, Show, Semigroup, Monoid)

(>+) :: ESC -> String -> ANSI
(>+) esc rhs = ANSI [(esc, T.pack rhs)]

start :: String -> ANSI
start s = ANSI [(ESC "", T.pack s)]

ansi :: Int -> ESC
ansi n = ESC ("\x1b[" <> T.pack (show n) <> "m")

reset, bold, dim, italic, underline, black, red, green, yellow, blue, magenta, cyan, white, dflt :: ESC
reset = ansi 0
bold = ansi 1
dim = ansi 2
italic = ansi 3
underline = ansi 4
black = ansi 90
red = ansi 91
green = ansi 92
yellow = ansi 93
blue = ansi 94
magenta = ansi 95
cyan = ansi 96
white = ansi 97
dflt = ansi 99

class Fmt a where
  fmt :: a -> String
  fmt e = let ANSI x = ansiFmt e in T.unpack (foldl (\txt (_, t) -> txt <> t) mempty x)
  termFmt :: a -> String
  termFmt e = let ANSI x = ansiFmt e in T.unpack (foldl (\txt (ESC code, t) -> ((txt <> code) <> t) <> unESC reset) mempty x)
  ansiFmt :: a -> ANSI

instance Fmt Expr where
  ansiFmt :: Expr -> ANSI
  ansiFmt e = case e of
    (Vbl x) -> green >+ x
    (App fn arg) -> red >+ "(" <> ansiFmt fn <> red >+ " " <> ansiFmt arg <> red >+ ")"
    (Lam x body) -> dim >+ "(λ" <> bold >+ x <> start "." <> ansiFmt body <> dim >+ ")"
    (Rec f body) -> dim >+ "(μ" <> bold >+ f <> start "." <> ansiFmt body <> dim >+ ")"
    (Op2 o left right) -> dim >+ "(" <> ansiFmt left <> start o <> ansiFmt right <> dim >+ ")"
    (Num n) -> start $ show n
    (If etest etrue efalse) -> start "(if " <> ansiFmt etest <> start " then " <> ansiFmt etrue <> start " else " <> ansiFmt efalse <> start ")"

instance (Show addr) => Fmt (Env addr) where
  ansiFmt :: (Show addr) => Env addr -> ANSI
  ansiFmt r = green >+ "⟦" <> fmt' r "" <> green >+ "⟧"
    where
      fmt' :: (Show addr) => Env addr -> String -> ANSI
      fmt' (Env ((v, a) : r')) sep = start sep <> green >+ v <> start "↦" <> green >+ show a <> fmt' (Env r') ","
      fmt' (Env []) _ = start ""

instance (Show addr, Fmt v) => Fmt (Store addr v) where
  ansiFmt s = yellow >+ "Σ⟦" <> fmt' s "" <> yellow >+ "⟧"
    where
      fmt' :: (Show addr, Fmt v) => Store addr v -> String -> ANSI
      fmt' (Store ((a, v) : r)) sep = start sep <> green >+ show a <> start "↦" <> ansiFmt v <> fmt' (Store r) ","
      fmt' (Store []) _ = start ""

instance (Fmt a, Fmt b) => Fmt (a, b) where
  ansiFmt (a, b) = start "(" <> ansiFmt a <> start ", " <> ansiFmt b <> start ")"
