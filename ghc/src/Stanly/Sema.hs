module Stanly.Sema (Stanly.Sema.eval) where

import Control.Monad.Identity ( Identity )
import Control.Monad.Reader (runReaderT, MonadReader, asks, ReaderT)
import Control.Monad.State (runState, StateT, gets, MonadState)
import Stanly.Eval (eval, Interpreter(..), Value(..), Store(..), Env(..))
import Stanly.Expr (Expr (..), Var, Fmt(..))
import Control.Monad.Fix (fix)

data Val
  = LamV Var Expr Env 
  | NumV Int
   deriving (Eq, Show)
  
newtype ConcreteT m a = ConcreteM { runConcreteT :: ReaderT Env (StateT (Store Val) m) a }
  deriving (Functor, Applicative, Monad, MonadReader Env, MonadState (Store Val), MonadFail)
type Concrete a = ConcreteT Identity a

eval :: Expr -> (Val, Store Val)
eval = (\(m::Concrete Val) -> runState (runReaderT (runConcreteT m) (Env [])) (Store [])) . fix Stanly.Eval.eval

instance Interpreter (ConcreteT Identity) Val where
  op2 o (NumV n0) (NumV n1) = case o of
    "+" -> return $ NumV (n0 + n1)
    "-" -> return $ NumV (n0 - n1)
    "*" -> return $ NumV (n0 * n1)
    "/" -> if n1 == 0 then error "division by zero" else return $ NumV (n0 `div` n1)
    _ -> error $ "Unknown operator '" ++ o ++ "'"
  op2 _ _ _ = error "argument to delta' must be NumV"
  lambda x body = asks (LamV x body);
  number n = return $ NumV n
  alloc _ _ = gets length

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