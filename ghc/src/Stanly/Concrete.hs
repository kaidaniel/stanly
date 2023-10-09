{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}
module Stanly.Concrete (Concrete) where

import Control.Monad.Except
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.Reader (MonadReader, ReaderT, asks, runReaderT)
import Control.Monad.State (MonadState, StateT, gets, runStateT)
import Control.Monad.Writer
import Stanly.Eval (Env (..), Interpreter (..), Store (..), bottom)
import Stanly.Expr (Expr (..), Var)
import Stanly.Fmt

data Val
  = LamV Var Expr (Env Int)
  | NumV Int
  deriving (Eq, Show)

type Env' = Env Int
type Store' = Store Int Val
type ScopeT = ReaderT Env'
type BottomT = ExceptT String
type FreshAddrT = StateT Store'

newtype ConcreteT m a = ConcreteT (ScopeT (BottomT (FreshAddrT m)) a)
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadError String,
      MonadReader (Env Int),
      MonadState (Store Int Val)
    )

type Concrete = ConcreteT Identity

instance Interpreter Concrete Val Int where
  op2 o (NumV n0) (NumV n1) = case o of
    "+" -> return $ NumV (n0 + n1)
    "-" -> return $ NumV (n0 - n1)
    "*" -> return $ NumV (n0 * n1)
    "/" ->
      if n1 == 0
        then bottom $ "Division by zero. " ++ show n0 ++ "/" ++ show n1
        else return $ NumV (n0 `div` n1)
    _ -> bottom $ unknownOp o
  op2 o _ _ = bottom $ invalidArgs o
  alloc _ = gets length
  run (ConcreteT m) =
    runIdentity (runStateT (runExceptT (runReaderT m (Env []))) (Store []))
  truthy (NumV n) = return (n /= 0)
  truthy _ = return False
  destruct :: Concrete Val -> Concrete (Expr, Maybe (Var, Env Int))
  destruct m = m >>= destruct'
    where
      destruct' (LamV x e r) = return (e, Just (x, r))
      destruct' (NumV n) = return (Num n, Nothing)
  construct (Lam x e) = Just $ asks (LamV x e)
  construct (Num n) = Just $ return (NumV n)
  construct _ = Nothing

instance Fmt Val where
  ansiFmt (LamV x body r) = start "λ" <> bold >+ x <> start "." <> ansiFmt body <> ansiFmt r
  ansiFmt (NumV n) = start $ show n

instance Fmt (Either String Val) where
  ansiFmt (Left err) = start err
  ansiFmt (Right val) = ansiFmt val

invalidArgs, unknownOp :: String -> String
invalidArgs o = "Invalid arguments to operator '" ++ o ++ "'"
unknownOp o = "Unknown operator '" ++ o ++ "'"

newtype Trace a = Trace (ConcreteT (WriterT [(Val, Store')] Identity) a)
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadError String,
      MonadReader Env',
      MonadState Store'
    )