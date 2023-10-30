{-# LANGUAGE LambdaCase #-}
module Stanly.Interpreter where

import Control.Monad.Reader (MonadReader(..), ReaderT)
import Control.Monad (liftM2, join)
import Text.Parsec
import Stanly.Fmt
import Data.Function((&))
import qualified Control.Applicative as A

type Var = String

newtype Env l = Env { unEnv :: [(Var, l)] } deriving (Eq, Show, Foldable)

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
  Rec f e      -> env >>= \r' -> alloc f >>= \l' -> ext l' $ assign f l' r' (ev e)
  App lamV x   -> ev lamV >>= \case
      LamV v' e' r' -> ev x >>= \x' -> alloc v' >>= \l' -> ext l' (pure x') >> assign v' l' r' (ev e')
      _             -> exc ("\"" <> fmt lamV <> "\" is not a function")

class Store l m where
  deref :: l -> m (Val l)
  ext   :: l -> m (Val l) -> m (Val l)
  alloc :: Var -> m l

class Environment l m where
  search :: Var -> (l -> m (Val l)) -> (String -> m (Val l)) -> m (Val l)
  assign :: Var -> l -> Env l -> m (Val l) -> m (Val l)
  env    :: m (Env l)

class Primops l m where
  op2    :: String -> Val l -> Val l -> m (Val l)
  branch :: m (Val l) -> m (Val l) -> Val l -> m (Val l)

class Exc m where
  exc :: String -> m (Val l)

class (Exc m, Primops l m, Store l m, Environment l m, Monad m) => Interpreter l m where
  ev :: Expr -> m (Val l)

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

instance (Monad m, Show l) => Environment l (ReaderT (Env l) m) where
  search variable iffound ifnotfound = ask >>= \r -> lookup variable (unEnv r) & \case 
    Just l -> iffound l
    _ -> ifnotfound (show variable <> " not found in environment: " <> fmt r)
  assign v l r = local (const (Env ((v, l) : unEnv r)))
  env = ask

subexprs :: (A.Alternative g) => Expr -> g Expr
subexprs = f
  where
  f = \case
    Lam _ e -> pure e A.<|> f e
    Num _ -> A.empty
    App fn x -> pure fn A.<|> pure x A.<|> f fn A.<|> f x
    Op2 _ l r -> pure l A.<|> pure r A.<|> f l A.<|> f r
    If b tru fls -> pure b A.<|> pure tru A.<|> pure fls A.<|> f b A.<|> f tru A.<|> f fls
    Rec _ e -> pure e A.<|> f e
    Vbl _ -> A.empty

newtype Store_ l = Store_ { unStore :: [(l, Val l)] } deriving (Eq, Show, Foldable)

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