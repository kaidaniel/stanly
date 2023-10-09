{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}
module Stanly.Concrete (Concrete) where

import Control.Monad.Except
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.Reader (MonadReader, ReaderT, asks, runReaderT)
import Control.Monad.State (MonadState, StateT, gets, runStateT)
import Stanly.Eval (Env (..), Interpreter (..), Store (..), Value (..), bottom)
import Stanly.Expr (Expr (..), Var)
import Stanly.Fmt

data Val
  = LamV Var Expr (Env Int)
  | NumV Int
  deriving (Eq, Show)

newtype Concrete a
  = Concrete (ReaderT (Env Int) (ExceptT String (StateT (Store Int Val) Identity)) a)
  deriving
    (Functor, Applicative, Monad, MonadError String, MonadReader (Env Int), MonadState (Store Int Val))

runConcrete :: Concrete a -> (Either String a, Store Int Val)
runConcrete (Concrete scope) = runIdentity (runStateT (runExceptT (runReaderT scope (Env []))) (Store []))

instance Interpreter Concrete Val Int where
  op2 o (NumV n0) (NumV n1) = case o of
    "+" -> return $ NumV (n0 + n1)
    "-" -> return $ NumV (n0 - n1)
    "*" -> return $ NumV (n0 * n1)
    "/" -> if n1 == 0 then bottom div0 else return $ NumV (n0 `div` n1)
    _ -> bottom $ unknownOp o
  op2 o _ _ = bottom $ invalidArgs o
  lambda x body = asks (LamV x body)
  number n = return $ NumV n
  alloc _ = gets length
  run (m :: Concrete Val) = runConcrete m
  truthy (NumV n) = return (n /= 0)
  truthy _ = return False

instance Value Val Int where
  destruct (LamV x e r) = (Just (x, r), e)
  destruct (NumV n) = (Nothing, Num n)

instance Fmt Val where
  ansiFmt (LamV x body r) = start "λ" <> bold >+ x <> start "." <> ansiFmt body <> ansiFmt r
  ansiFmt (NumV n) = start $ show n

instance Fmt (Either String Val) where
  ansiFmt (Left err) = start err
  ansiFmt (Right val) = ansiFmt val

div0 :: String
div0 = "Division by zero"

invalidArgs, unknownOp :: String -> String
invalidArgs o = "Invalid arguments to operator '" ++ o ++ "'"
unknownOp o = "Unknown operator '" ++ o ++ "'"
