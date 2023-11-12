{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}
{-# LANGUAGE LambdaCase #-}
module Stanly.Concrete(execConcrete, execTrace, execNotCovered, execPruned) where

import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.Reader(MonadReader, ReaderT, runReaderT)
import Control.Monad.State(gets, get, MonadState, modify, StateT, runStateT)
import Control.Monad.Except ( throwError , MonadError, ExceptT, runExceptT)
import Stanly.Interpreter
import Stanly.Fmt
import Data.Function(fix)
import Control.Monad.Writer(WriterT, runWriterT, tell, MonadWriter)
import Data.List ((\\))
import qualified Stanly.Interpreter as S
import Control.Arrow((>>>))

-- newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a       }
-- newtype ExceptT e m a = ExceptT { runExceptT :: m (Either e a) }
-- newtype StateT  s m a = StateT  { runStateT  :: s -> m (a, s)  }
-- ConcreteT l v e m :: * -> *
-- = \a::*.{C} {R} Env l -> {E} {S} Store_ l v -> m (Either e a, Store_ l)
-- type ConcreteT l e m = ReaderT (Env l) (ExceptT e (StateT (Store_ l) m))
type Addr = Int
type Store' = Store_ Int
newtype ConcreteT m a = ConcreteT (ReaderT (Env Int) (ExceptT String (StateT Store' m)) a)
  deriving (Functor, Applicative, Monad, MonadReader (Env Int), MonadState Store', MonadError String, MonadWriter r, Environment Int)

runConcreteT :: ConcreteT m a -> m (Either String a, Store')
runConcreteT (ConcreteT m) = (flip runStateT (Store_ []) . runExceptT) (runReaderT m (Env []))

execConcrete' :: (Expr -> ConcreteT Identity (S.Val Int)) -> Expr -> (Either String (Val Int), Store')
execConcrete' ev' = runIdentity . runConcreteT . ev'

execConcrete :: Expr -> (Either String (Val Int), Store')
execConcrete = execConcrete' (fix S.eval')

instance (Monad m) => Exc (ConcreteT m) where
  exc er = throwError $ "Exception: " ++ er

instance (Monad m) => Primops Addr (ConcreteT m) where
  op2 o (NumV n0) (NumV n1) = case o of
    "+" -> return $ NumV (n0 + n1)
    "-" -> return $ NumV (n0 - n1)
    "*" -> return $ NumV (n0 * n1)
    "/" ->
      if n1 == 0
        then exc $ "Division by zero. " ++ show n0 ++ "/" ++ show n1
        else return $ NumV (n0 `div` n1)
    _ -> exc $ unknownOp o
  op2 "+" (TxtV t0) (TxtV t1) = return $ TxtV (t0 ++ t1)
  op2 "+" (TxtV t0) (NumV n1) = return $ TxtV (t0 ++ show n1)
  op2 o a b = exc (invalidOperands o a b)
  branch fls tru condition = case condition of
    NumV n -> if n /= 0 then tru else fls
    _ -> pure $ Undefined "Branching on non-numeric value"

instance (Monad m) => Store Addr (ConcreteT m) where
  alloc _ = gets length
  deref l = do
    (Store_ store) <- get
    case lookup l store of
      Just val -> return val
      Nothing -> error $ show l ++ " not found in store. " ++ fmt (Store_ store)
  ext l s = modify (\(Store_ store) -> Store_ ((l, s) : store))

instance Interpreter Addr (ConcreteT Identity) where
  ev :: Expr -> ConcreteT Identity (Val Int)
  ev = eval

unknownOp :: String -> String
unknownOp o = "Unknown operator '" ++ o ++ "'"

invalidOperands :: (Fmt a1, Fmt a2) => String -> a1 -> a2 -> String
invalidOperands o a b =
  "Invalid arguments to operator '"
  <> o
  <> "':\n"
  <> "\nleft operand  >>> "
  <> termFmt a
  <> "\noperation     >>> "
  <> o
  <> "\nright operand >>> "
  <> termFmt b

newtype ProgramTrace = ProgramTrace [(Expr, Env Int, Store')] deriving (Eq, Show, Semigroup, Monoid)

instance Fmt ProgramTrace where
  ansiFmt :: ProgramTrace -> ANSI
  ansiFmt (ProgramTrace li) = join' (zip (map ansiFmt li) [1 ..])
    where
      join' :: [(ANSI, Integer)] -> ANSI
      join' = foldr (\(a, n) b -> start (show @Integer n ++ ". ") <> a <> start "\n" <> b) mempty


instance (Monad m) => Interpreter Int (ConcreteT (WriterT ProgramTrace m)) where
  ev e = do
    r <- env
    store <- get
    tell $ ProgramTrace [(e, r, store)]
    eval e

evTrace :: (MonadState Store' m, MonadWriter ProgramTrace m, Interpreter Addr m,  Environment Int m) => Expr -> m (Val Addr)
evTrace e = do
    r <- env
    store <- get
    tell $ ProgramTrace [(e, r, store)]
    S.eval' evTrace e

execTrace :: Expr -> ProgramTrace
execTrace = snd . runIdentity . runWriterT . runConcreteT . evTrace

newtype NotCovered = NotCovered [Expr] deriving (Eq, Show, Semigroup, Monoid)
instance Fmt NotCovered where
  ansiFmt (NotCovered li) = foldr ((\a b -> a <> start "\n" <> b) . ansiFmt) mempty li

execNotCovered :: Expr -> NotCovered
execNotCovered e = NotCovered $ let ProgramTrace t = execTrace e in (e : subexprs e) \\ map (\(e', _, _) -> e') t


evPruned :: S.Expr -> ConcreteT Identity (S.Val Int)
evPruned e = S.eval' evPruned e >>= \case S.LamV x body r -> return (S.LamV x body (S.pruneEnv body r)); v -> return v

execPruned :: Expr -> (Either String (Val Int), Store')
execPruned = execConcrete' evPruned