module Stanly.ConcreteSemantics (Interpreter, Value (..), Stanly.ConcreteSemantics.eval) where

import Control.Monad.Except
import Control.Monad.Fail
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Stanly.Eval (eval)
import Stanly.Expr (Addr, Env (..), Expr (..), Value (..), Var, assign')
import Language.Haskell.TH.Lib (varE, varT)

instance MonadFail Identity where
  fail = error

type Store = [(Addr, Value)]

type InterpreterM = ReaderT Env (State Store)

type Interpreter = InterpreterM Value


run' :: Interpreter -> (Value, Store)
run' m = runState (runReaderT m (Env [])) []

find' :: Env -> Var -> Interpreter
find' (Env r) x = case lookup x r of
  Just a -> gets (\s -> case lookup a s of Just x -> x; Nothing -> error $ "Address '" ++ show a ++ "' not found in store")
  Nothing -> error $ "Variable '" ++ x ++ "' not found in environment"

ext' :: Int -> Value -> InterpreterM ()
ext' a v = modify ((a, v) :)

alloc' f = gets length

delta' o n0 n1 = case o of
  "+" -> return $ NumV (n0 + n1)
  "-" -> return $ NumV (n0 - n1)
  "*" -> return $ NumV (n0 * n1)
  "/" -> if n1 == 0 then fail "division by zero" else return $ NumV (n0 `div` n1)
  _ -> fail $ "Unknown operator '" ++ o ++ "'"

isZero' v = return (v == 0)

eval = Stanly.Eval.eval run' ask local asks find' ext' alloc' assign' isZero' delta'


class InterpreterC m v where
  -- isZero :: Integer -> m Bool
  -- eval' :: Expr -> m v
  -- alloc :: Var -> m Int
  delta :: String -> Integer -> Integer -> m v
  find :: Env -> Var -> m v
  ext :: Int -> v -> m ()


instance InterpreterC InterpreterM Value where
  -- eval' = Stanly.ConcreteSemantics.eval
  find = find'
  ext = ext'
  delta = delta'
  -- alloc = alloc' 
  -- isZero = isZero'


