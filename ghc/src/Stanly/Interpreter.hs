{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use >=>" #-}
module Stanly.Interpreter where

import Control.Monad.Reader (MonadReader(..), ReaderT)
import Stanly.Expr(Expr (..), Var)
import Stanly.Fmt
import Control.Monad (liftM2, join, ap)

eval :: (Interpreter l m) => Expr -> m (Val l)
eval = \case
  Num n        -> pure $ NumV n
  Lam v e      -> fmap (LamV v e) env
  Vbl vbl      -> search exc vbl >>= deref
  If b tru fls -> ev b >>= branch (ev fls) (ev tru)
  Op2 o e0 e1  -> join $ liftM2 (op2 o) (ev e0) (ev e1)
  Rec f e      -> env >>= \mr -> alloc f >>= \ml -> ext ml $ assign (f, ml) mr (ev e)
  App lamV x   -> ev lamV >>= \case
      LamV mv me mr -> ev x >>= \mx -> alloc mv >>= \ml -> ext ml (pure mx) >> assign (mv, ml) mr (ev me)
      _             -> exc $ "\"" <> fmt lamV <> "\" is not a function"

newtype Store_ l = Store_ [(l, Val l)] deriving (Eq, Show, Foldable)
newtype Env l = Env { unEnv :: [(Var, l)] } deriving (Eq, Show, Foldable)

data Val l
  = LamV Var Expr (Env l)
  | NumV Int
  | Undefined String
  deriving (Eq, Show, Foldable)

class Store l m where
  deref :: l -> m (Val l)
  ext   :: l -> m (Val l) -> m (Val l)
  alloc :: Var -> m l

class (Monad m) => Environment env l m | m -> l, m -> env where
  search :: (forall a. String -> m a) -> Var -> m l
  assign :: (Var, l) -> env -> m (Val l) -> m (Val l)
  env    :: m env

class Exc m where
  exc :: String -> m a

class Primops l m where
  op2    :: String -> Val l -> Val l -> m (Val l)
  branch :: m (Val l) -> m (Val l) -> Val l -> m (Val l)

instance (Monad m, Show l) => Environment (Env l) l (ReaderT (Env l) m) where
  search f v = ask >>= \(Env r) -> maybe (f $ show v <> " not found in environment: " <> fmt (Env r)) return (lookup v r)
  assign (v, l) r = local (const (Env ((v, l) : unEnv r)))
  env = ask

class (Exc m, Show l, Eq l, Primops l m, Store l m, Environment (Env l) l m, Monad m) => Interpreter l m where
  ev :: Expr -> m (Val l)

instance (Show l) => Fmt (Env l) where
  ansiFmt :: (Show l) => Env l -> ANSI
  ansiFmt r = green >+ "⟦" <> fmt' r "" <> green >+ "⟧"
    where
      fmt' :: (Show l) => Env l -> String -> ANSI
      fmt' (Env ((v, a) : r')) sep = start sep <> green >+ v <> start "↦" <> green >+ show a <> fmt' (Env r') ","
      fmt' (Env []) _ = start ""

instance (Show l) => Fmt (Store_ l) where
  ansiFmt s = yellow >+ "Σ⟦" <> fmt' s "" <> yellow >+ "⟧"
    where
      fmt' :: (Show l) => Store_ l -> String -> ANSI
      fmt' (Store_ ((a, v) : r)) sep = start sep <> green >+ show a <> start "↦" <> ansiFmt v <> fmt' (Store_ r) ","
      fmt' (Store_ []) _ = start ""

instance (Show l) => Fmt (Val l) where
  ansiFmt = \case
    LamV x body r -> start "λ" <> bold >+ x <> start "." <> ansiFmt body <> ansiFmt r
    NumV n -> start $ show n
    Undefined s -> start $ "Undefined: " <> s

instance (Show l) => Fmt (Either String (Val l)) where
  ansiFmt = \case
    Left err -> start err
    Right val -> ansiFmt val