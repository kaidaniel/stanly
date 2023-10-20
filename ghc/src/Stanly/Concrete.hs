{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}
module Stanly.Concrete(Concrete, execConcrete, execTrace) where

import Control.Monad.Except
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.Reader
import Control.Monad.State
import Stanly.Eval
import Stanly.Expr (Expr (..), Var)
import Stanly.Fmt
import Control.Monad.Writer

data Val
  = LamV Var Expr (Env Int)
  | NumV Int
  deriving (Eq, Show)

type Env' = Env Int
type Store' = Store Int Val
type ScopeT = ReaderT Env'
type BottomT = ExceptT String
type FreshAddrT = StateT Store'
type MonadScope = MonadReader Env'
type MonadBottom = MonadError String
type MonadFreshAddr = MonadState Store'

type ConcreteT m = ScopeT (BottomT (FreshAddrT m))
type Concrete = ConcreteT Identity

op2Num :: MonadBottom m => String -> Val -> Val -> m Val
op2Num o (NumV n0) (NumV n1) = case o of
  "+" -> return $ NumV (n0 + n1)
  "-" -> return $ NumV (n0 - n1)
  "*" -> return $ NumV (n0 * n1)
  "/" ->
    if n1 == 0
      then bottom $ "Division by zero. " ++ show n0 ++ "/" ++ show n1
      else return $ NumV (n0 `div` n1)
  _ -> bottom $ unknownOp o
op2Num o _ _ = bottom $ invalidArgs o

truthyNum :: Monad m => Val -> m Bool
truthyNum (NumV n) = return (n /= 0)
truthyNum _ = return False

allocFresh :: MonadFreshAddr m => Var -> m Int
allocFresh _ = gets length

destructVal :: Monad m => m Val -> m (Expr, Maybe (Var, Env Int))
destructVal m = m >>= destructVal'
    where
      destructVal' (LamV x e r) = return (e, Just (x, r))
      destructVal' (NumV n) = return (Num n, Nothing)

constructVal :: MonadScope m => Expr -> Maybe (m Val)
constructVal (Lam x e) = Just $ asks (LamV x e)
constructVal (Num n) = Just $ return (NumV n)
constructVal _ = Nothing

runConcreteT :: ConcreteT m a -> m (Either String a, Store')
runConcreteT m = runStateT (runExceptT (runReaderT m (Env []))) (Store [])

execConcrete :: Expr -> (Either String Val, Store')
execConcrete = runIdentity . runConcreteT . ev

instance Interpreter Concrete Val Int where
  op2 = op2Num
  alloc = allocFresh
  truthy = truthyNum
  destruct = destructVal
  construct = constructVal

instance Fmt Val where
  ansiFmt (LamV x body r) = start "λ" <> bold >+ x <> start "." <> ansiFmt body <> ansiFmt r
  ansiFmt (NumV n) = start $ show n

instance Fmt (Either String Val) where
  ansiFmt (Left err) = start err
  ansiFmt (Right val) = ansiFmt val

invalidArgs, unknownOp :: String -> String
invalidArgs o = "Invalid arguments to operator '" ++ o ++ "'"
unknownOp o = "Unknown operator '" ++ o ++ "'"

type TraceT m = ConcreteT (WriterT [(Expr, Env', Store')] m)

evTrace :: (Interpreter m Val Int,  MonadWriter [(Expr, Env', Store')] m) => Expr -> m Val
evTrace expr = do
  env <- ask
  store <- get
  tell [(expr, env, store)]
  eval expr
  

instance Interpreter (TraceT Identity) Val Int where
  op2 = op2Num
  alloc = allocFresh
  truthy = truthyNum
  destruct = destructVal
  construct = constructVal
  ev = evTrace
  
runTraceT :: ConcreteT (WriterT w m) a -> m ((Either String a, Store'), w)
runTraceT m = runWriterT (runConcreteT m)

execTrace :: Expr -> [(Expr, Env', Store')]
execTrace expr = let (_, trace) = (runIdentity . runTraceT . ev) expr in trace
