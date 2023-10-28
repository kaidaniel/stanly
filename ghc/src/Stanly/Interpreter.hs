module Stanly.Interpreter where

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.State (MonadState, StateT, runStateT)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import Stanly.Expr(Expr, Var)
import Stanly.Fmt

newtype Store addr val = Store [(addr, val)] deriving (Eq, Show, Foldable)
newtype Env addr = Env [(Var, addr)] deriving (Eq, Show)

class Lattice m where
    bottom :: String -> m a
    top :: String -> m a

class (Fmt val, Lattice m) => Value val m where
    op2 :: String -> val -> val -> m val
    truthy :: val -> m Bool

class (Show addr, Eq addr, Value val m, MonadState (Store addr val) m, MonadReader (Env addr) m) => Memory addr val m where
    alloc :: Var -> m addr
    construct :: Expr -> Maybe (m val)
    destruct :: m val -> m(Expr, Maybe (Var, Env addr))
    
class (Memory addr val m) => Interpreter addr val m where
    ev :: Expr -> m val

type InterpreterT addr val extremum m = ReaderT (Env addr) (ExceptT extremum (StateT (Store addr val) m))

runInterpreterT :: InterpreterT addr val extremum m a -> m (Either extremum a, Store addr val)
runInterpreterT m = runStateT (runExceptT (runReaderT m (Env []))) (Store [])

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