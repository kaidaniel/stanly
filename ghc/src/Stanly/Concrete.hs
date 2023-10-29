{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}
module Stanly.Concrete(execConcrete, execTrace, execNotCovered) where

import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.Reader(ask, MonadReader, ReaderT, runReaderT)
import Control.Monad.State(gets, get, MonadState, modify, StateT, runStateT)
import Control.Monad.Except ( throwError , MonadError, ExceptT, runExceptT)
import Stanly.Eval(eval)
import Stanly.Interpreter
import Stanly.Expr
import Stanly.Fmt
import Control.Monad.Writer(WriterT, runWriterT, tell, MonadWriter)
import Data.List ((\\))

-- newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a       }
-- newtype ExceptT e m a = ExceptT { runExceptT :: m (Either e a) }
-- newtype StateT  s m a = StateT  { runStateT  :: s -> m (a, s)  }
-- ConcreteT l v e m :: * -> *
-- = \a::*.{C} {R} Env l -> {E} {S} Store_ l v -> m (Either e a, Store_ l)
-- type ConcreteT l e m = ReaderT (Env l) (ExceptT e (StateT (Store_ l) m))
newtype ConcreteT m a = ConcreteT (ReaderT (Env Int) (ExceptT String (StateT (Store_ Int) m)) a)
  deriving (Functor, Applicative, Monad, MonadReader (Env Int), MonadState (Store_ Int), MonadError String, MonadWriter r)

runConcreteT :: ConcreteT m a -> m (Either String a, Store_ Int)
runConcreteT (ConcreteT m) = (flip runStateT (Store_ []) . runExceptT) (runReaderT m (Env []))

execConcrete :: Expr -> (Either String (Val Int), Store_ Int)
execConcrete = runIdentity . runConcreteT . ev

instance (Monad m) => Exc (ConcreteT m) where
  exc er = throwError $ "Exception: " ++ er

instance (Monad m) => Primops Int (ConcreteT m) where
  op2 o (NumV n0) (NumV n1) = case o of
    "+" -> return $ NumV (n0 + n1)
    "-" -> return $ NumV (n0 - n1)
    "*" -> return $ NumV (n0 * n1)
    "/" ->
      if n1 == 0
        then exc $ "Division by zero. " ++ show n0 ++ "/" ++ show n1
        else return $ NumV (n0 `div` n1)
    _ -> exc $ unknownOp o
  op2 o _ _ = exc $ invalidArgs o
  truthy (NumV n) = return (n /= 0)
  truthy _ = return False

instance (Monad m) => Store Int (ConcreteT m) where
  alloc _ = gets length
  find l = do
    (Store_ store) <- get
    case lookup l store of
      Just val -> return val
      Nothing -> error $ show l ++ " not found in store. " ++ fmt (Store_ store)
  ext l m = m >>= (\s -> modify (\(Store_ store) -> Store_ ((l, s) : store))) >> m
    -- memkpy binding = modify (\(Store_ store) -> Store_ (binding : store))

instance Interpreter Int (ConcreteT Identity) where
  ev = eval

invalidArgs, unknownOp :: String -> String
invalidArgs o = "Invalid arguments to operator '" ++ o ++ "'"
unknownOp o = "Unknown operator '" ++ o ++ "'"

newtype ProgramTrace = ProgramTrace [(Expr, Env Int, Store_ Int)] deriving (Eq, Show, Semigroup, Monoid)

instance Fmt ProgramTrace where
  ansiFmt :: ProgramTrace -> ANSI
  ansiFmt (ProgramTrace li) = join' (zip (map ansiFmt li) [1 ..])
    where
      join' :: [(ANSI, Integer)] -> ANSI
      join' = foldr (\(a, n) b -> start (show @Integer n ++ ". ") <> a <> start "\n" <> b) mempty


instance (Monad m) => Interpreter Int (ConcreteT (WriterT ProgramTrace m)) where
  ev e = do
    env <- ask
    store <- get
    tell $ ProgramTrace [(e, env, store)]
    eval e

runTraceT :: ConcreteT (WriterT w m) a -> m ((Either String a, Store_ Int), w)
runTraceT m = runWriterT (runConcreteT m)

execTrace :: Expr -> ProgramTrace
execTrace e = snd ((runIdentity . runTraceT . ev) e)

newtype NotCovered = NotCovered [Expr] deriving (Eq, Show, Semigroup, Monoid)
instance Fmt NotCovered where
  ansiFmt (NotCovered li) = foldr ((\a b -> a <> start "\n" <> b) . ansiFmt) mempty li

execNotCovered :: Expr -> NotCovered
execNotCovered e = NotCovered $ let ProgramTrace t = execTrace e in (e : strictSubexprs e) \\ map (\(e', _, _) -> e') t
