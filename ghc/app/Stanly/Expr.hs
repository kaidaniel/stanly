{-# LANGUAGE DeriveGeneric #-}

module Stanly.Expr (Expr (..), Value (..), Env (..), Var, Addr, assign, emptyEnv) where
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
  deriving (Generic, Eq)

instance Show Expr where
  show (Vbl x) = x
  show (App fn arg) = "(" ++ show fn ++ " " ++ show arg ++ ")"
  show (Lam x body) = "(λ" ++ x ++ "." ++ show body ++ ")"
  show (Rec f body) = "(μ" ++ f ++ "." ++ show body ++ ")"
  show (Op2 o left right) = "(" ++ show left ++ " " ++ o ++ " " ++ show right ++ ")"
  show (Num n) = show n
  show (If etest etrue efalse) = "(if " ++ show etest ++ " then " ++ show etrue ++ " else " ++ show efalse ++ ")"

type Addr = Int

newtype Env = Env [(Var, Addr)] deriving (Eq)

data Value = LamV Var Expr Env | NumV Integer deriving (Eq)

emptyEnv = Env []

instance Show Value where
  show (LamV x body r) = "λ" ++ x ++ "." ++ show body ++ " " ++ show r
  show (NumV n) = show n

instance Show Env where
  show env = "⟦" ++ showrec env "" ++ "⟧"
    where
      showrec (Env ((x, a) : r)) sep = sep ++ x ++ "→ " ++ show a ++ showrec (Env r) " "
      showrec (Env []) _ = ""

assign var addr (Env env) = Env ((var, addr) : env)
