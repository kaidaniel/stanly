{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}
module Stanly.Concrete(execConcrete, execTrace, execNotCovered) where

import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.Reader(ask, asks)
import Control.Monad.State(gets, get)
import Stanly.Eval(eval)
import Stanly.Interpreter
import Stanly.Expr ( Expr(Num, Lam), Var, strictSubexprs )
import Stanly.Fmt ( (>+), bold, start, ANSI, Fmt(ansiFmt) )
import Control.Monad.Writer(WriterT, runWriterT, tell)
import Data.List ((\\))
import Control.Monad.Except (throwError)

data Val
  = LamV Var Expr (Env Int)
  | NumV Int
  deriving (Eq, Show)

type ConcreteT m = InterpreterT Int Val String m

execConcrete :: Expr -> (Either String Val, Store Int Val)
execConcrete = runIdentity . runInterpreterT . ev

instance (Monad m) => Lattice (ConcreteT m) where
  bottom err = throwError $ "Bottom: " ++ err
  top err = error $ "Concrete semantics does not have a top element. " ++ err

instance (Monad m) => Value Val (ConcreteT m) where
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
  truthy (NumV n) = return (n /= 0)
  truthy _ = return False

instance (Monad m) => Memory Int Val (ConcreteT m) where
  alloc _ = gets length
  construct (Lam x e) = Just $ asks (LamV x e)
  construct (Num n) = Just $ return (NumV n)
  construct _ = Nothing
  destruct m = m >>= destruct'
    where
      destruct' (LamV x e r) = return (e, Just (x, r))
      destruct' (NumV n) = return (Num n, Nothing)

instance Interpreter Int Val (ConcreteT Identity) where
  ev = eval

instance Fmt Val where
  ansiFmt (LamV x body r) = start "λ" <> bold >+ x <> start "." <> ansiFmt body <> ansiFmt r
  ansiFmt (NumV n) = start $ show n

instance Fmt (Either String Val) where
  ansiFmt (Left err) = start err
  ansiFmt (Right val) = ansiFmt val

invalidArgs, unknownOp :: String -> String
invalidArgs o = "Invalid arguments to operator '" ++ o ++ "'"
unknownOp o = "Unknown operator '" ++ o ++ "'"

newtype ProgramTrace = ProgramTrace [(Expr, Env Int, Store Int Val)] deriving (Eq, Show, Semigroup, Monoid)

instance Fmt ProgramTrace where
  ansiFmt :: ProgramTrace -> ANSI
  ansiFmt (ProgramTrace li) = join' (zip (map ansiFmt li) [1 ..])
    where
      join' :: [(ANSI, Integer)] -> ANSI
      join' = foldr (\(a, n) b -> start (show @Integer n ++ ". ") <> a <> start "\n" <> b) mempty

instance (Monad m) => Interpreter Int Val (ConcreteT (WriterT ProgramTrace m)) where
  ev e = do
    env <- ask
    store <- get
    tell $ ProgramTrace [(e, env, store)]
    eval e

runTraceT :: ConcreteT (WriterT w m) a -> m ((Either String a, Store Int Val), w)
runTraceT m = runWriterT (runInterpreterT m)

execTrace :: Expr -> ProgramTrace
execTrace e = snd ((runIdentity . runTraceT . ev) e)

newtype NotCovered = NotCovered [Expr] deriving (Eq, Show, Semigroup, Monoid)
instance Fmt NotCovered where
  ansiFmt (NotCovered li) = foldr ((\a b -> a <> start "\n" <> b) . ansiFmt) mempty li

execNotCovered :: Expr -> NotCovered
execNotCovered e = NotCovered $ let ProgramTrace t = execTrace e in (e : strictSubexprs e) \\ map (\(e', _, _) -> e') t
