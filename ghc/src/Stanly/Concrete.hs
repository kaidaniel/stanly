module Stanly.Concrete (Concrete) where

import Control.Monad.Reader (runReaderT, MonadReader, asks, ReaderT)
import Control.Monad.State (runState, State, gets, MonadState)
import Stanly.Eval (Interpreter(..), Value(..), Store(..), Env(..))
import Stanly.Expr (Expr (..), Var)
import Stanly.Fmt (Fmt(fmt))

data Val
  = LamV Var Expr Env 
  | NumV Int
   deriving (Eq, Show)
  
newtype Concrete a = Concrete { runConcrete :: ReaderT Env (State (Store Val)) a}
  deriving (Functor, Applicative, Monad, MonadReader Env, MonadState (Store Val))

instance Interpreter Concrete Val where
  op2 o (NumV n0) (NumV n1) = case o of
    "+" -> return $ NumV (n0 + n1)
    "-" -> return $ NumV (n0 - n1)
    "*" -> return $ NumV (n0 * n1)
    "/" -> if n1 == 0 then error "division by zero" else return $ NumV (n0 `div` n1)
    _ -> error $ "Unknown operator '" ++ o ++ "'"
  op2 _ _ _ = error "argument to op2 must be NumV"
  lambda x body = asks (LamV x body);
  number n = return $ NumV n
  alloc _ = gets length
  run :: Concrete Val -> (Val, Store Val)
  run (m::Concrete Val) = runState (runReaderT (runConcrete m) (Env [])) (Store [])

instance Value Val where
  var (LamV x _ _) = x
  var (NumV _) = error "intended to be impossible at run time"
  expr (LamV _ e _) = e
  expr (NumV n) = Num n
  truthy (NumV n) = n /= 0
  truthy _ = False
  env (LamV _ _ r) = r
  env (NumV _) = Env []

instance Fmt Val where
  fmt (LamV x body r) = "λ" ++ x ++ "." ++ fmt body ++ " " ++ fmt r
  fmt (NumV n) = show n