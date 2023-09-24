{-# LANGUAGE DeriveGeneric #-}

module Stanly.Expr (Expr (..), Value (..), Env (..), Var, Addr, assign, emptyEnv, unparse) where
import GHC.Generics

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


unparse :: Expr -> String
unparse (Vbl x) = x
unparse (App fn arg) = "(" ++ unparse fn ++ " " ++ unparse arg ++ ")"
unparse (Lam x body) = "(λ" ++ x ++ "." ++ unparse body ++ ")"
unparse (Rec f body) = "(μ" ++ f ++ "." ++ unparse body ++ ")"
unparse (Op2 o left right) = "(" ++ unparse left ++ " " ++ o ++ " " ++ unparse right ++ ")"
unparse (Num n) = show n
unparse (If etest etrue efalse) = "(if " ++ unparse etest ++ " then " ++ unparse etrue ++ " else " ++ unparse efalse ++ ")"

type Addr = Int

newtype Env = Env [(Var, Addr)] deriving (Eq)

data Value = LamV Var Expr Env | NumV Integer deriving (Eq)

emptyEnv = Env []

instance Show Value where
  show (LamV x body r) = "λ" ++ x ++ "." ++ unparse body ++ " " ++ show r
  show (NumV n) = show n

instance Show Env where
  show env = "⟦" ++ showrec env "" ++ "⟧"
    where
      showrec (Env ((x, a) : r)) sep = sep ++ x ++ "→" ++ show a ++ showrec (Env r) " "
      showrec (Env []) _ = ""

assign var addr (Env env) = Env ((var, addr) : env)
