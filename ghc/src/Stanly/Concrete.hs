{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}
module Stanly.Concrete(execConcrete, execTrace, execNotCovered) where

import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.Reader(ask, asks)
import Control.Monad.State(gets, get)
import Stanly.Eval(eval)
import Stanly.Interpreter(bottom, runInterpreterT, MonadScope, MonadBottom, MonadStore, MonadInterpreter (..), InterpreterT, Env (..), Store (..))
import Stanly.Expr ( Expr(Num, Lam), Var, strictSubexprs )
import Stanly.Fmt ( (>+), bold, start, ANSI, Fmt(ansiFmt) )
import Control.Monad.Writer(WriterT, runWriterT, tell)
import Data.List ((\\))

data Val
  = LamV Var Expr (Env Int)
  | NumV Int
  deriving (Eq, Show)


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

allocFresh :: (MonadStore Int Val m) => Var -> m Int
allocFresh _ = gets length

destructVal :: Monad m => m Val -> m (Expr, Maybe (Var, Env Int))
destructVal m = m >>= destructVal'
    where
      destructVal' (LamV x e r) = return (e, Just (x, r))
      destructVal' (NumV n) = return (Num n, Nothing)

constructVal :: (MonadScope Int m) => Expr -> Maybe (m Val)
constructVal (Lam x e) = Just $ asks (LamV x e)
constructVal (Num n) = Just $ return (NumV n)
constructVal _ = Nothing

execConcrete :: Expr -> (Either String Val, Store Int Val)
execConcrete = runIdentity . runInterpreterT . ev

instance MonadInterpreter Int Val (InterpreterT Int Val Identity) where
  op2 = op2Num
  alloc = allocFresh
  truthy = truthyNum
  destruct = destructVal
  construct = constructVal
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

type TraceT m = InterpreterT Int Val (WriterT ProgramTrace m)

instance MonadInterpreter Int Val (TraceT Identity) where
  op2 = op2Num
  alloc = allocFresh
  truthy = truthyNum
  destruct = destructVal
  construct = constructVal
  ev e = do
    env <- ask
    store <- get
    tell $ ProgramTrace [(e, env, store)]
    eval e

runTraceT :: InterpreterT Int Val (WriterT w m) a -> m ((Either String a, Store Int Val), w)
runTraceT m = runWriterT (runInterpreterT m)

execTrace :: Expr -> ProgramTrace
execTrace e = snd ((runIdentity . runTraceT . ev) e)

newtype NotCovered = NotCovered [Expr] deriving (Eq, Show, Semigroup, Monoid)
instance Fmt NotCovered where
  ansiFmt (NotCovered li) = foldr ((\a b -> a <> start "\n" <> b) . ansiFmt) mempty li

execNotCovered :: Expr -> NotCovered
execNotCovered e = NotCovered $ let ProgramTrace t = execTrace e in (e : strictSubexprs e) \\ map (\(e', _, _) -> e') t
