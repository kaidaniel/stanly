module Stanly.Interpreter where

import Control.Monad.Except (MonadError, throwError)
import Control.Monad.State (MonadState)
import Control.Monad.Reader (MonadReader)
import Stanly.Expr(Expr, Var)
import Stanly.Fmt

bottom :: (MonadError String m) => String -> m a
bottom err = throwError $ "Bottom: " ++ err
newtype Store addr val = Store [(addr, val)] deriving (Eq, Show, Foldable)
newtype Env addr = Env [(Var, addr)] deriving (Eq, Show)

class
  (Fmt val, Show addr, Eq addr, MonadState (Store addr val) m, MonadReader (Env addr) m, MonadError String m) =>
  Interpreter m val addr where
  op2 :: String -> val -> val -> m val
  truthy :: val -> m Bool
  alloc :: Var -> m addr
  ev :: Expr -> m val
  destruct :: m val -> m (Expr, Maybe (Var, Env addr))
  construct :: Expr -> Maybe (m val)


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