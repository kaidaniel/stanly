module Stanly.ConcreteSemantics (Concrete, Stanly.ConcreteSemantics.eval) where

import Control.Monad.Except
import Control.Monad.Fail
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Stanly.Eval (eval, Interpreter(..))
import Stanly.Expr (Addr, Env (..), Expr (..), Value (..), Var, assign')
import Language.Haskell.TH.Lib (varE, varT)

instance MonadFail Identity where
  fail = error

type Store = [(Addr, Value)]

type ConcreteM r = ReaderT r (State Store)

type Concrete = ConcreteM Env Value


run' :: Concrete -> (Value, Store)
run' m = runState (runReaderT m (Env [])) []

eval = run' . fix Stanly.Eval.eval

instance Interpreter (ConcreteM Env) Value where
  find (Env r) x = case lookup x r of
    Just a -> gets (\s -> case lookup a s of Just x -> x; Nothing -> error $ "Address '" ++ show a ++ "' not found in store")
    Nothing -> error $ "Variable '" ++ x ++ "' not found in environment"
  ext a v = modify ((a, v) :)
  evOp2 :: String -> Value -> Value -> ConcreteM Env Value
  evOp2 o (NumV n0) (NumV n1) = case o of
    "+" -> return $ NumV (n0 + n1)
    "-" -> return $ NumV (n0 - n1)
    "*" -> return $ NumV (n0 * n1)
    "/" -> if n1 == 0 then fail "division by zero" else return $ NumV (n0 `div` n1)
    _ -> fail $ "Unknown operator '" ++ o ++ "'"
  evOp2 _ _ _ = fail "argument to delta' must be NumV"
  alloc f = gets length
  truthy v = case v of (NumV n) -> return (n == 0); _ -> return False
  env = ask
  inEnv menv mvalue = do { r <- menv; local (const r) mvalue; }
  bindVar var addr env = return $ assign' var addr env
  returnClosure x body = LamV x body <$> env
  getClosure v = case v of (LamV x body r) -> return (x, body, r); _ -> fail "argument to getClosure must be LamV"
  literalValue e = case e of (Num n) -> return $ NumV n; (Lam x body) -> LamV x body <$> env ; _ -> fail "argument to literalValue must be Num or Lam"
