module Stanly.Monads(Interpreter, Value(..), run, local, asks, ask, find, ext, alloc, assign, isZero, delta) where

import Stanly.Expr(Expr(..), Var)

import Control.Monad.Writer
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Fail
import Control.Monad.Identity
instance MonadFail Identity where
    fail = error

type Addr = Int
newtype Env = Env [(Var, Addr)] deriving (Eq)
data Value = LamV Var Expr Env | NumV Integer deriving (Eq)
type Store = [(Addr, Value)]
type InterpreterM = ReaderT Env (State Store)
type Interpreter = InterpreterM Value

assign var addr (Env env) = Env ((var, addr):env)

emptyEnv = Env []
emptyStore = []

instance Show Value where
    show (LamV x body r) = "λ" ++ x ++ "." ++ show body ++ " " ++ show r
    show (NumV n) = show n

instance Show Env where
    show env = "⟦" ++ showrec env "" ++ "⟧"
        where 
            showrec (Env ((x, a):r)) sep = sep ++ x ++ "→ " ++ show a ++ showrec (Env r) " "
            showrec (Env []) _ = ""

run :: Interpreter -> (Value, Store)
run m = runState (runReaderT m emptyEnv) emptyStore

find :: Env -> Var -> Interpreter
find (Env r) x = case lookup x r of
    Just a -> gets (\s -> case lookup a s of Just x -> x; Nothing -> error $ "Address '" ++ show a ++ "' not found in store")
    Nothing -> error $ "Variable '" ++ x ++ "' not found in environment"

ext :: Int -> Value -> InterpreterM ()
ext a v = modify ((a, v):)
alloc f = gets length
delta o n0 n1 = case o of
    "+" -> return $ NumV (n0 + n1)
    "-" -> return $ NumV (n0 - n1)
    "*" -> return $ NumV (n0 * n1)
    "/" -> if n1 == 0 then fail "division by zero" else return $ NumV (n0 `div` n1)
    _ -> fail $ "Unknown operator '" ++ o ++ "'"
isZero v = return (v == 0)