module Stanly.Expr(Expr(..), Var) where

type Var = String
data Expr 
    = Vbl Var
    | App Expr Expr 
    | Lam Var Expr
    | Rec Var Expr 
    | Op2 String Expr Expr 
    | Num Integer 
    | If Expr Expr Expr deriving 
    (Eq)

instance Show Expr where
    show (Vbl x) = x
    show (App fn arg) = "(" ++ show fn ++ " " ++ show arg ++ ")"
    show (Lam x body) = "(λ" ++ x ++ "." ++ show body ++ ")"
    show (Rec f body) = "(μ" ++ f ++ "." ++ show body ++ ")"
    show (Op2 o left right) = "(" ++ show left ++ " " ++ o ++ " " ++ show right ++ ")"
    show (Num n) = show n
    show (If etest etrue efalse) = "(if " ++ show etest ++ " then " ++ show etrue ++ " else " ++ show efalse ++ ")"