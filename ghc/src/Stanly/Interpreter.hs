module Stanly.Interpreter where

import Control.Monad.Except (MonadError, throwError, ExceptT, runExceptT)
import Control.Monad.State (MonadState, StateT, runStateT)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import Stanly.Expr(Expr, Var)
import Stanly.Fmt

newtype Store addr val = Store [(addr, val)] deriving (Eq, Show, Foldable)
newtype Env addr = Env [(Var, addr)] deriving (Eq, Show)

type MonadScope addr = MonadReader (Env addr)
type MonadStore addr val = MonadState (Store addr val)
type MonadBottom = MonadError String

class
  (Fmt val, Show addr, Eq addr, MonadStore addr val m, MonadScope addr m, MonadBottom m) =>
  MonadInterpreter addr val m where
  op2 :: String -> val -> val -> m val
  truthy :: val -> m Bool
  alloc :: Var -> m addr
  ev :: Expr -> m val
  destruct :: m val -> m (Expr, Maybe (Var, Env addr))
  construct :: Expr -> Maybe (m val)

type InterpreterT addr val m = ReaderT (Env addr) (ExceptT String (StateT (Store addr val) m))

runInterpreterT :: InterpreterT addr val m a -> m (Either String a, Store addr val)
runInterpreterT m = runStateT (runExceptT (runReaderT m (Env []))) (Store [])

bottom :: (MonadBottom m) => String -> m a
bottom err = throwError $ "Bottom: " ++ err

instance (Show addr) => Fmt (Env addr) where
  ansiFmt :: (Show addr) => Env addr -> ANSI
  ansiFmt r = green >+ "⟦" <> fmt' r "" <> green >+ "⟧"
    where
      fmt' :: (Show addr) => Env addr -> String -> ANSI
      fmt' (Env ((v, a) : r')) sep = start sep <> green >+ v <> start "↦" <> green >+ show a <> fmt' (Env r') ","
      fmt' (Env []) _ = start ""

instance (Show addr, Fmt v) => Fmt (Store addr v) where
  ansiFmt s = yellow >+ "Σ⟦" <> fmt' s "" <> yellow >+ "⟧"
    where
      fmt' :: (Show addr, Fmt v) => Store addr v -> String -> ANSI
      fmt' (Store ((a, v) : r)) sep = start sep <> green >+ show a <> start "↦" <> ansiFmt v <> fmt' (Store r) ","
      fmt' (Store []) _ = start ""