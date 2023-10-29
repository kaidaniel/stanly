{-# LANGUAGE FunctionalDependencies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use >=>" #-}
module Stanly.Interpreter where

import Control.Monad.Reader (MonadReader(..), ReaderT)
import Stanly.Expr(Expr (..), Var)
import Stanly.Fmt
import Control.Monad (liftM2, join)
import Data.Bool (bool)

eval :: (Interpreter l m) => Expr -> m (Val l)
eval expression = case expression of
  (Num n)            -> pure (NumV n)
  (Lam x e)          -> fmap (LamV x e) env
  (Vbl vbl)          -> search exc vbl >>= deref
  (If test tru fls)  -> ev test >>= \t -> truthy t >>= ev . bool fls tru
  (Op2 o left right) -> join $ liftM2 (op2 o) (ev left) (ev right)
  (Rec f e)          -> env >>= \mr    -> alloc f   >>= \ml -> ext ml (assign (f, ml) mr (ev e))
  (App lamV x)       -> ev lamV >>= \mlamV -> case mlamV of
      (LamV mx' me mr) -> ev x >>= \mx -> alloc mx' >>= \ml -> ext ml (pure mx) >> assign (mx', ml) mr (ev me)
      _                -> env >>= exc . notAFunction mlamV
  where
    notAFunction lamV r = "\"" <> fmt lamV <> "\" is not a function. " <> fmt expression <> fmt r

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
  truthy :: Val l -> m Bool

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
  ansiFmt (LamV x body r) = start "λ" <> bold >+ x <> start "." <> ansiFmt body <> ansiFmt r
  ansiFmt (NumV n) = start $ show n
  ansiFmt (Undefined s) = start ("Undefined: " ++ s)

instance (Show l) => Fmt (Either String (Val l)) where
  ansiFmt (Left err) = start err
  ansiFmt (Right val) = ansiFmt val