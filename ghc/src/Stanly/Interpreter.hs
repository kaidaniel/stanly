{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Use fmap" #-}
module Stanly.Interpreter where

import qualified Control.Monad.Reader as R
import qualified Control.Monad as M
import qualified Text.Parsec as P
import qualified Stanly.Fmt as F
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
  | Txt String
  | If Expr Expr Expr
  deriving (Eq, Show)

data Val l
  = LamV Var Expr (Env l)
  | NumV Integer
  | TxtV String
  | Undefined String
  deriving (Eq, Show, Foldable)

eval :: (Interpreter l m) => Expr -> m (Val l)
eval = \case
  Num n        -> pure  (NumV n)
  Txt s        -> pure  (TxtV s)
  Lam v e      -> fmap  (LamV v e) env
  Vbl vbl      -> search vbl deref exc
  If b tru fls -> (=<<) (branch (ev fls) (ev tru)) (ev b)
  Op2 o e0 e1  -> bind2 (op2 o) (ev e0) (ev e1)
  Rec f t      -> do                   -- Γ Σ | eval[mu f.(fn x. ..f..)]
      env'  <- env
      l     <- alloc f                 -- Σ[l -> ...]
      resv  <- assign f l env' (ev t)  -- Γ[f -> l] Σ[l -> ...]          | ev[fn x.(..f..)]
      ext l resv                       -- Γ[f -> l] Σ[l -> fn x.(..f..)] | fn x.(..f..)
      pure resv
  App lamV arg  -> ev lamV >>= \case
      LamV x body r -> do                -- Γ Σ | eval[(r | (fn x.(..x..))) arg ]
          evArg <- ev arg                -- Γ Σ | ev[arg]
          allocX <- alloc x
          ext allocX evArg               -- Σ[lx -> ev[arg]]
          assign x allocX r (ev body)    -- r[x -> lx] Σ[lx -> ev[arg]] | ev[(..x..)]
      _             -> exc (notAFunction lamV arg)
  where
    bind2 f a b   = M.join $ M.liftM2 f a b
    notAFunction lamV arg = 
      "Left hand side of application not bound to a function."
      <> "\n\nIn function position >>> " <> F.fmt lamV
      <> "\nIn argument position >>> " <> F.fmt arg

class Store l m where
  deref :: l -> m (Val l)
  ext   :: l -> Val l -> m ()
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

parser :: String -> String -> Either P.ParseError Expr
parser = P.parse $ expr <* P.eof
  where
  expr = ws *> expr'
  expr' = parens (P.try op2' P.<|> P.try app P.<|> expr)
    P.<|> A.liftA  Txt  stringLiteral
    P.<|> A.liftA2 Lam  (do try' "λ" P.<|> try' "fn "; iden) (do dot; expr)
    P.<|> A.liftA2 Rec  (do try' "μ" P.<|> try' "mu "; iden) (do dot; expr)
    P.<|> A.liftA3 If   (do kw "if" ; expr) (do kw "then"; expr) (do kw "else"; expr)
    P.<|> A.liftA3 let_ (do kw "let"; iden) (do kw "="   ; expr) (do kw "in" P.<|> kw ";"; expr)
    P.<|> A.liftA  Num nat
    P.<|> A.liftA  Vbl iden
  op2'= A.liftA3 (flip Op2) expr operator expr
  app = A.liftA  (foldl1 App) (P.many1 expr)
  let_ x arg body = App (Lam x body) arg
  lx = Tn.makeTokenParser emptyDef
    { Tn.commentStart = "/*", Tn.commentEnd = "*/", Tn.commentLine = "//"
    , Tn.identLetter = Tn.identLetter emptyDef A.<|> P.oneOf "-"
    , Tn.opStart = P.oneOf "+-/*" , Tn.opLetter = P.oneOf "+-/*"
    , Tn.reservedNames = ["let", "in", "if", "then", "else", ";"]}
  kw     = Tn.reserved lx; try'     = P.try . Tn.symbol lx;   ws = Tn.whiteSpace lx
  parens = Tn.parens   lx; iden     = Tn.identifier lx; dot      = Tn.dot      lx
  nat    = Tn.natural  lx; operator = Tn.operator   lx;
  stringLiteral = Tn.stringLiteral lx

instance (Monad m, Show l) => Environment l (R.ReaderT (Env l) m) where
  search variable iffound ifnotfound = R.ask >>= \r -> lookup variable (unEnv r) & \case
    Just l -> iffound l
    _ -> ifnotfound (show variable <> " not found in environment: " <> F.fmt r)
  assign v l r = R.local (const (Env ((v, l) : unEnv r)))
  env = R.ask

subexprs :: (A.Alternative g) => Expr -> g Expr
subexprs = f
  where
  f expression = A.asum $ case expression of
    Lam _ e        -> p [e] <> [f e]
    Num _          -> []
    Txt _          -> []
    App fn x       -> p [fn, x] <> [f fn, f x]
    Op2 _ l r      -> p [l , r] <> [f l , f r]
    If b tru fls   -> p [b, tru, fls] <> [f b, f tru, f fls]
    Rec _ e        -> p [e] <> [f e]
    Vbl _          -> []
  p = map pure


newtype Store_ l = Store_ { unStore :: [(l, Val l)] } deriving (Eq, Show, Foldable)

instance (Show l) =>F.Fmt(Env l) where
  ansiFmt r = F.green F.>+ "⟦" <> fmt' r "" <> F.green F.>+ "⟧"
    where
      fmt' (Env ((v, a) : r')) sep = F.start sep <> F.green F.>+ v <> F.start ": " <> F.green F.>+ show a <> fmt' (Env r') ", "
      fmt' (Env []) _ = F.start ""

instance (Show l) => F.Fmt(Store_ l) where
  ansiFmt s = F.yellow F.>+ "Σ⟦" <> fmt' s "" <> F.yellow F.>+ "⟧"
    where
      fmt' (Store_ ((a, v) : r)) sep = F.start sep <> F.green F.>+ show a <> F.start ": " <> F.ansiFmt v <> fmt' (Store_ r) ", "
      fmt' (Store_ []) _ = F.start ""

instance (Show l) => F.Fmt(Val l) where
  ansiFmt = \case
    LamV x body r -> F.start "λ" <> F.bold F.>+ x <> F.start "." <> F.ansiFmt body <> F.ansiFmt r
    NumV n        -> F.dim F.>+ show n
    TxtV s        -> F.dim F.>+ show s
    Undefined s   -> F.start $ "Undefined: " <> s

instance (Show l) => F.Fmt(Either String (Val l)) where
  ansiFmt = \case
    Left err  -> F.start err
    Right val -> F.ansiFmt val

instance F.Fmt Expr where
  ansiFmt = \case
    Vbl x            -> F.green F.>+ x
    App fn arg       -> F.red F.>+ "("  <> appParen fn    <> F.start " " <> F.ansiFmt arg    <> F.red F.>+ ")"
    Lam x body       -> F.dim F.>+ "(λ" <> F.bold F.>+ x  <> F.start "." <> binderParen body <> F.dim F.>+ ")"
    Rec f body       -> F.dim F.>+ "(μ" <> F.bold F.>+ f  <> F.start "." <> binderParen body <> F.dim F.>+ ")"
    Op2 o left right -> F.dim F.>+ "("  <> F.ansiFmt left <> opFmt o     <> F.ansiFmt right  <> F.dim F.>+ ")"
    Num n            -> F.start $ show n
    Txt s            -> F.dim F.>+ show s
    If etest etrue efalse ->
                        F.start "(if "   <> F.ansiFmt etest  <>
                        F.start " then " <> F.ansiFmt etrue  <>
                        F.start " else " <> F.ansiFmt efalse <> F.start ")"
    where
      appParen = \case
          App fn arg -> appParen fn <> F.red F.>+ " " <> F.ansiFmt arg
          e -> F.ansiFmt e
      binderParen = \case
          Rec f body -> F.start "μ" <> F.bold F.>+ f <> F.start "." <> binderParen body
          Lam x body -> F.start "λ" <> F.bold F.>+ x <> F.start "." <> binderParen body
          e -> F.ansiFmt e
      opFmt o = F.start (" " <> o <> " ")