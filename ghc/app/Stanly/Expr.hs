{-# LANGUAGE DeriveGeneric #-}

module Stanly.Expr (Expr (..), Value (..), Env (..), Var, Addr, assign', fmt) where
import GHC.Generics
import Control.Monad.Fix (fix)

type Var = String

data Expr
  = Vbl Var
  | App Expr Expr
  | Lam Var Expr
  | Rec Var Expr
  | Op2 String Expr Expr
  | Num Integer
  | If Expr Expr Expr
  deriving (Generic, Eq, Show)

assign' var addr (Env env) = Env ((var, addr) : env)

type Addr = Int
newtype Env = Env [(Var, Addr)] deriving (Eq, Show)
data Value = LamV Var Expr Env | NumV Integer deriving (Eq, Show)


class Fmt a where
  fmt :: a -> String

instance Fmt Value where
  fmt (LamV x body r) = "λ" ++ x ++ "." ++ fmt body ++ " " ++ fmt r
  fmt (NumV n) = show n

instance Fmt Env where
  fmt env = "⟦" ++ fmt env "" ++ "⟧"
    where
      fmt (Env ((x, a) : r)) sep = sep ++ x ++ "→" ++ show a ++ fmt (Env r) " "
      fmt (Env []) _ = ""

instance Fmt Expr where
  fmt (Vbl x) = x
  fmt (App fn arg) = "(" ++ fmt fn ++ " " ++ fmt arg ++ ")"
  fmt (Lam x body) = "(λ" ++ x ++ "." ++ fmt body ++ ")"
  fmt (Rec f body) = "(μ" ++ f ++ "." ++ fmt body ++ ")"
  fmt (Op2 o left right) = "(" ++ fmt left ++ " " ++ o ++ " " ++ fmt right ++ ")"
  fmt (Num n) = show n
  fmt (If etest etrue efalse) = "(if " ++ fmt etest ++ " then " ++ fmt etrue ++ " else " ++ fmt efalse ++ ")"