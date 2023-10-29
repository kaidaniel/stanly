{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
module Stanly.Interpreter(
  parser, eval, strictSubexprs, Var, Expr(..), Env(..), Store_(..), Val(..), Interpreter(..), Store(..), Environment(..), Exc(..), Primops(..)
  ) where

import Control.Monad.Reader (MonadReader(..), ReaderT)
import Control.Monad (liftM2, join)
import Text.Parsec
import Stanly.Fmt
import Data.Function((&))

type Var  = String

data Expr
  = Vbl Var
  | App Expr Expr
  | Lam Var Expr
  | Rec Var Expr
  | Op2 String Expr Expr
  | Num Int
  | If Expr Expr Expr
  deriving (Eq, Show)

data Val l
  = LamV Var Expr (Env l)
  | NumV Int
  | Undefined String
  deriving (Eq, Show, Foldable)

eval :: (Interpreter l m) => Expr -> m (Val l)
eval = \case
  Num n        -> pure $ NumV n
  Lam v e      -> fmap (LamV v e) env
  Vbl vbl      -> search vbl deref exc
  If b tru fls -> ev b >>= branch (ev fls) (ev tru)
  Op2 o e0 e1  -> join $ liftM2 (op2 o) (ev e0) (ev e1)
  Rec f e      -> env >>= \mr -> alloc f >>= \ml -> ext ml $ assign f ml mr (ev e)
  App lamV x   -> ev lamV >>= \case
      LamV mv me mr -> ev x >>= \mx -> alloc mv >>= \ml -> ext ml (pure mx) >> assign mv ml mr (ev me)
      _             -> exc ("\"" <> fmt lamV <> "\" is not a function")

class Store l m where
  deref :: l -> m (Val l)
  ext   :: l -> m (Val l) -> m (Val l)
  alloc :: Var -> m l

class (Monad m) => Environment env l m | m -> l, m -> env where
  search :: Var -> (l -> m (Val l)) -> (String -> m (Val l)) -> m (Val l)
  assign :: Var -> l -> env -> m (Val l) -> m (Val l)
  env    :: m env

class Primops l m where
  op2    :: String -> Val l -> Val l -> m (Val l)
  branch :: m (Val l) -> m (Val l) -> Val l -> m (Val l)

class Exc m where
  exc :: String -> m (Val l)

class (Exc m, Primops l m, Store l m, Environment (Env l) l m) => Interpreter l m where
  ev :: Expr -> m (Val l)

-- | Turn program text into an AST.
parser :: String -> String -> Either ParseError Expr
parser = parse (expr <* eof)
  where
  expr =
    try ((\x v k -> App (Lam x k) v) <$> kw "let" ident <*> kw "=" expr <*> kw "in" expr)
    <|> between (char '(') (char ')') (ws exprNoParens)
    <|> ws atom
  exprNoParens =
    try (Lam <$> binder ["λ", "fn "] <*> expr)
    </> (Rec <$> binder ["μ", "mu "] <*> expr)
    </> (If <$> kw "if" expr <*> kw "then" expr <*> kw "else" expr)
    </> (App <$> ws expr <*> expr)
    </> (flip Op2 <$> expr <*> op <*> expr)
    <|> atom
  atom =
        (Num <$> integer)
    <|> (Vbl <$> ident)
  op = ws (choice [string x | x <- ["+", "-", "*", "/"]])
  binder c = (choice [string x | x <- c]) *> ws (ident <* char '.')
  (</>) l r = l <|> try r
  kw w e = string w >> (lookAhead (char '(') <|> space) >> ws e
  integer = read <$> many1 digit
  ws e = spaces *> e <* spaces
  ident = many1 letter

instance (Monad m, Show l) => Environment (Env l) l (ReaderT (Env l) m) where
  search variable iffound ifnotfound = ask >>= \r -> lookup variable (unEnv r) & \case 
    Just l -> iffound l
    _ -> ifnotfound (show variable <> " not found in environment: " <> fmt r)
  assign v l r = local (const (Env ((v, l) : unEnv r)))
  env = ask

strictSubexprs :: Expr -> [Expr]
strictSubexprs = f
  where
  f = \case
    Lam _ e -> e : f e
    Num _ -> []
    App fn x -> fn : x : f fn ++ f x
    Op2 _ l r -> l : r : f l ++ f r
    If b tru fls -> b : tru : fls : f b ++ f tru ++ f fls
    Rec _ e -> e : f e
    Vbl _ -> []

newtype Store_ l = Store_ [(l, Val l)] deriving (Eq, Show, Foldable)
newtype Env l = Env { unEnv :: [(Var, l)] } deriving (Eq, Show, Foldable)

instance (Show l) => Fmt (Env l) where
  ansiFmt r = green >+ "⟦" <> fmt' r "" <> green >+ "⟧"
    where
      fmt' (Env ((v, a) : r')) sep = start sep <> green >+ v <> start "↦" <> green >+ show a <> fmt' (Env r') ","
      fmt' (Env []) _ = start ""

instance (Show l) => Fmt (Store_ l) where
  ansiFmt s = yellow >+ "Σ⟦" <> fmt' s "" <> yellow >+ "⟧"
    where
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

instance Fmt Expr where
  ansiFmt = \case
    Vbl x -> green >+ x
    App fn arg -> red >+ "(" <> ansiFmt fn <> red >+ " " <> ansiFmt arg <> red >+ ")"
    Lam x body -> dim >+ "(λ" <> bold >+ x <> start "." <> ansiFmt body <> dim >+ ")"
    Rec f body -> dim >+ "(μ" <> bold >+ f <> start "." <> ansiFmt body <> dim >+ ")"
    Op2 o left right -> dim >+ "(" <> ansiFmt left <> start o <> ansiFmt right <> dim >+ ")"
    Num n -> start $ show n
    If etest etrue efalse -> start "(if " <> ansiFmt etest <> start " then " <> ansiFmt etrue <> start " else " <> ansiFmt efalse <> start ")"