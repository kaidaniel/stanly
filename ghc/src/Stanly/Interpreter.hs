module Stanly.Interpreter where

import Control.Monad.Reader (MonadReader)
import Stanly.Expr(Expr, Var)
import Stanly.Fmt

newtype Store_ l = Store_ [(l, Val l)] deriving (Eq, Show, Foldable)
newtype Env l = Env { unEnv :: [(Var, l)] } deriving (Eq, Show, Foldable)

data Val l
  = LamV Var Expr (Env l)
  | NumV Int
  | Undefined String
  deriving (Eq, Show, Foldable)

class Store l m where
    find :: l -> m (Val l)
    ext :: l -> m (Val l) -> m (Val l)
    alloc :: Var -> m l

class Exc m where
    exc :: String -> m a

class Primops l m where
    op2 :: String -> Val l -> Val l -> m (Val l)
    truthy :: Val l -> m Bool


class (Exc m, Show l, Eq l, Primops l m, Store l m, MonadReader (Env l) m) => Interpreter l m where
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