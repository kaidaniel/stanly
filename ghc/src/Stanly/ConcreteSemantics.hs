module Stanly.ConcreteSemantics (Stanly.ConcreteSemantics.eval) where

import Control.Monad.Except ( fix )
import Control.Monad.Identity ( Identity )
import Control.Monad.Reader
import Control.Monad.State ( runState, StateT, gets, MonadState)
import Stanly.Eval (eval, Interpreter(..), Value(..), Store, Env(..))
import Stanly.Expr (Expr (..), Var, Fmt(..))

data Value 
  = LamV Var Expr Env 
  | NumV Int
   deriving (Eq, Show)
  
newtype ConcreteT r m a = ConcreteM { runConcreteM :: ReaderT r (StateT (Store Stanly.ConcreteSemantics.Value) m) a }
  deriving (Functor, Applicative, Monad, MonadReader r, MonadState (Store Stanly.ConcreteSemantics.Value), MonadFail)
type Concrete r a = ConcreteT r Identity a

eval :: Expr -> (Stanly.ConcreteSemantics.Value, Store Stanly.ConcreteSemantics.Value)
eval = (\(m::Concrete Env Stanly.ConcreteSemantics.Value) -> runState (runReaderT (runConcreteM m) (Env [])) []) . fix Stanly.Eval.eval

instance Interpreter (ConcreteT Env Identity) Stanly.ConcreteSemantics.Value where
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

instance Stanly.Eval.Value Stanly.ConcreteSemantics.Value where
  var (LamV x _ _) = x
  var (NumV _) = error "intended to be impossible at run time"
  expr (LamV _ e _) = e
  expr (NumV n) = Num n
  truthy (NumV n) = n /= 0
  truthy _ = False
  env (LamV _ _ r) = r
  env (NumV _) = Env []


instance Fmt Stanly.ConcreteSemantics.Value where
  fmt (LamV x body r) = "λ" ++ x ++ "." ++ fmt body ++ " " ++ fmt r
  fmt (NumV n) = show n