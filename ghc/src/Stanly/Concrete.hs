{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
module Stanly.Concrete (Concrete) where

import Control.Monad.Reader (runReaderT, MonadReader, asks, ReaderT)
import Control.Monad.State (runStateT, StateT, gets, MonadState)
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.Except
import Stanly.Eval (Interpreter(..), Value(..), Store(..), Env(..))
import Stanly.Expr (Expr (..), Var)
import Stanly.Fmt

data Val
  = LamV Var Expr (Env Int)
  | NumV Int
   deriving (Eq, Show)


newtype Concrete a = 
  Concrete (ReaderT (Env Int) (ExceptT String (StateT (Store Int Val) Identity)) a)
  deriving
  (Functor, Applicative, Monad, MonadReader (Env Int), MonadState (Store Int Val))
runConcrete (Concrete scope) = runIdentity (runStateT (runExceptT (runReaderT scope (Env []))) (Store []))

instance Interpreter Concrete Val Int where
  op2 o (NumV n0) (NumV n1) = case o of
    "+" -> return $ NumV (n0 + n1)
    "-" -> return $ NumV (n0 - n1)
    "*" -> return $ NumV (n0 * n1)
    "/" -> if n1 == 0 then error div0 else return $ NumV (n0 `div` n1)
    _ -> error $ unknownOp o
  op2 o _ _ = error $ invalidArgs o
  lambda x body = asks (LamV x body);
  number n = return $ NumV n
  alloc _ = gets length
  run (m::Concrete Val) = runConcrete m
  truthy (NumV n) = return (n /= 0)
  truthy _ = return False

instance Value Val Int where
  var (LamV x _ _) = x
  var (NumV _) = error "'Value.var' expects LamV input, not NumV"
  expr (LamV _ e _) = e
  expr (NumV n) = Num n
  env (LamV _ _ r) = r
  env (NumV _)  = error "'Value.env' expects LamV input, not NumV"

instance Fmt Val where
  ansiFmt (LamV x body r) = start "λ" <> bold >+ x <> start "." <> ansiFmt body <> ansiFmt r
  ansiFmt (NumV n) = start $ show n

div0 :: String
div0 = "Division by zero"

invalidArgs, unknownOp :: String -> String
invalidArgs o = "Invalid arguments to operator '" ++ o ++ "'"
unknownOp o = "Unknown operator '" ++ o ++ "'"
