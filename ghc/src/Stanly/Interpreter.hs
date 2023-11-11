{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module Stanly.Interpreter where

import Control.Monad.Reader (MonadReader(..), ReaderT)
import Control.Monad (liftM3, liftM2, join)
import Text.Parsec
import Stanly.Fmt
import Data.Function((&))
import qualified Control.Applicative as A
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Tn

type Var = String

newtype Env l = Env { unEnv :: [(Var, l)] } deriving (Eq, Show, Foldable)

data Expr
  = Vbl Var
  | App Expr Expr
  | Lam Var Expr
  | Rec Var Expr
  | Op2 String Expr Expr
  | Num Integer
  | If Expr Expr Expr
  deriving (Eq, Show)

data Val l
  = LamV Var Expr (Env l)
  | NumV Integer
  | Undefined String
  deriving (Eq, Show, Foldable)

eval :: (Interpreter l m) => Expr -> m (Val l)
eval = \case
  Num n        -> pure (NumV n)
  Lam v e      -> fmap (LamV v e) env
  Vbl vbl      -> search vbl deref exc
  If b tru fls -> plug3 branch (ev fls) (ev tru) (ev b)
  Op2 o e0 e1  -> bind2 (op2 o) (ev e0) (ev e1)
  Rec f e      -> (env, alloc f) >>== \r' l' -> ext l' (assign f l' r' (ev e))
  App lamV x   -> ev lamV >>= \case
      LamV v' e' r' -> (ev x, alloc v') >>== \x' l' -> do ext l' (pure x'); assign v' l' r' (ev e')
      _             -> exc ("\"" <> fmt lamV <> "\" is not a function")
  where
    (x, y) >>== f = x >>= \a -> y >>= \b -> f a b
    plug3 f a b c = f a b =<< c
    bind2 f a b = join $ liftM2 f a b

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
parser = parse $ expr <* eof
  where
  expr = ws *>
    parens (
      try (liftM3 (flip Op2) expr op expr)
      </> liftM2 App expr expr
      </> expr)
    </> liftM2 Lam (do symbol "λ" <|> symbol "fn "; iden) (do dot; expr)
    </> liftM2 Rec (do symbol "μ" <|> symbol "mu "; iden) (do dot; expr)
    <|> liftM3 If     (do kw "if" ; expr) (do kw "then"; expr) (do kw "else"; expr)
    <|> liftM3 letApp (do kw "let"; iden) (do kw "="   ; expr) (do kw "in"  ; expr)
    <|> fmap   Num nat
    <|> fmap   Vbl iden
  letApp x arg body = App (Lam x body) arg
  lx = Tn.makeTokenParser emptyDef { Tn.commentStart = "/*", Tn.commentEnd = "*/"
    , Tn.commentLine = "//", Tn.opStart = oneOf "+-/*" , Tn.opLetter = oneOf "+-/*"
    , Tn.reservedNames = ["let", "in", "if", "then", "else"]}
  a </> b = a <|> try b
  parens = Tn.parens lx; iden = Tn.identifier lx; ws = Tn.whiteSpace lx; symbol = Tn.symbol lx
  nat = Tn.natural lx; dot = Tn.dot lx; op = Tn.operator lx; kw = Tn.reserved lx


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
