{-# LANGUAGE OverloadedStrings #-}

module Stanly.Fmt where

import Stanly.Expr(Expr(..))
import Stanly.Eval(Env(..), Store(..))

class Fmt a where
  fmt :: a -> String

instance Fmt Expr where
  fmt (Vbl x) = x
  fmt (App fn arg) = "(" ++ fmt fn ++ " " ++ fmt arg ++ ")"
  fmt (Lam x body) = "(λ" ++ x ++ "." ++ fmt body ++ ")"
  fmt (Rec f body) = "(μ" ++ f ++ "." ++ fmt body ++ ")"
  fmt (Op2 o left right) = "(" ++ fmt left ++ " " ++ o ++ " " ++ fmt right ++ ")"
  fmt (Num n) = show n
  fmt (If etest etrue efalse) = "(if " ++ fmt etest ++ " then " ++ fmt etrue ++ " else " ++ fmt efalse ++ ")"

instance Fmt addr => Fmt (Env addr) where
  fmt r = "ℾ⟦" ++ fmt' r "" ++ "⟧"
    where
        fmt' (Env ((v, a) : r)) sep = sep ++ show v ++ "↦" ++ fmt a ++ fmt' (Env r) ","
        fmt' (Env []) _ = ""


instance (Show addr, Fmt v) => Fmt (Store addr v) where
    fmt :: Show addr => Store addr v -> String
    fmt s = "⅀⟦" ++ fmt' s "" ++ "⟧"
        where
            fmt' (Store ((a, v) : r)) sep = sep ++ show a ++ "↦" ++ fmt v ++ fmt' (Store r) ","
            fmt' (Store []) _ = ""

instance Fmt Int where
  fmt = show