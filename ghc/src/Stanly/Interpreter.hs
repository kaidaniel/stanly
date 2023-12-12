{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}

module Stanly.Interpreter where

import Control.Applicative qualified as A
import Control.Arrow ((>>>))
import Data.Coerce (coerce)
import Data.Function ((&))
import Data.List qualified as L
import GHC.Generics
import Stanly.Fmt qualified as F
import Stanly.Unicode
import Text.Parsec qualified as P
import Text.Parsec.Language (emptyDef)
import Text.Parsec.Token qualified as Tn
import Text.Printf qualified as Pr

type Var = String

newtype Env l = Env [(Var, l)] deriving (Eq, Show, Foldable, Semigroup, Monoid)

newtype Store l = Store [(l, Val l)] deriving (Eq, Show, Foldable, Semigroup, Monoid)

data Expr
    = Vbl Var
    | App Expr Expr
    | Lam Var Expr
    | Rec Var Expr
    | Op2 String Expr Expr
    | Num Integer
    | Txt String
    | If Expr Expr Expr
    deriving (Eq, Show, Generic)

data Val l
    = LamV Var Expr (Env l)
    | NumV Integer
    | TxtV String
    | Undefined String
    deriving (Eq, Show, Foldable)

type Eval m l = Expr → m (Val l)
type Eval' m l = Eval m l → Eval m l
type Eval'' m l = Eval' m l → Eval' m l

eval ∷ ∀ m l. (Show l, Monad m) ⇒ Interpreter l m → Eval' m l
eval Interpreter{..} ev = \case
    Num n → 𝖕 (NumV n)
    Txt s → 𝖕 (TxtV s)
    Lam v e → 𝖕 (LamV v e) ⊛ env
    Vbl vbl → search vbl deref exc
    If b tru fls → (=<<) (branch (ev fls) (ev tru)) (ev b)
    Op2 o e0 e1 → 𝖏𝖋𝖋 (op2 o) (ev e0) (ev e1)
    Rec f t → do
        l ← alloc f
        resv ← localEnv' ([(f, l)] <>) (ev t)
        updateStore' ([(l, resv)] <>)
        𝖕 resv
    App lamV arg →
        ev lamV >>= \case
            LamV x body r → do
                evArg ← ev arg
                allocX ← alloc x
                updateStore' ([(allocX, evArg)] <>)
                localEnv (const (coerce [(x, allocX)] <> r)) (ev body)
            _ → exc (notAFunction lamV arg)
  where
    notAFunction lamV arg =
        "Left hand side of application not bound to a function."
            <> "\n\nIn function position >>> "
            <> F.fmt lamV
            <> "\nIn argument position >>> "
            <> F.fmt arg
    search variable iffound ifnotfound =
        env >>= \r →
            lookup variable (coerce r) & \case
                Just l → iffound l
                _ → ifnotfound (show variable <> " not found in environment: " <> F.fmt r)
    localEnv' f = localEnv (coerce f)
    updateStore' f = updateStore (coerce f)

data Interpreter l m = Interpreter
    { deref ∷ l → m (Val l)
    , env ∷ m (Env l)
    , localEnv ∷ (Env l → Env l) → m (Val l) → m (Val l)
    , store ∷ m (Store l)
    , updateStore ∷ (Store l → Store l) → m ()
    , alloc ∷ Var → m l
    , op2 ∷ String → Val l → Val l → m (Val l)
    , branch ∷ m (Val l) → m (Val l) → Val l → m (Val l)
    , exc ∷ String → m (Val l)
    }

parser ∷ String → String → Either P.ParseError Expr
parser = P.parse $ expr <* P.eof
  where
    expr = ws *> expr'
    expr' =
        parens (P.try op2' P.<|> P.try app P.<|> expr)
            P.<|> 𝖕 Txt ⊛ stringLiteral
            P.<|> 𝖕 Lam ⊛ (do try' "λ" P.<|> try' "fn "; iden) ⊛ (do dot; expr)
            P.<|> 𝖕 Rec ⊛ (do try' "μ" P.<|> try' "mu "; iden) ⊛ (do dot; expr)
            P.<|> 𝖕 If ⊛ (do kw "if"; expr) ⊛ (do kw "then"; expr) ⊛ (do kw "else"; expr)
            P.<|> 𝖕 let_ ⊛ (do kw "let"; iden) ⊛ (do kw "="; expr) ⊛ (do kw "in" P.<|> kw ";"; expr)
            P.<|> 𝖕 Num ⊛ nat
            P.<|> 𝖕 Vbl ⊛ iden
    op2' = 𝖋𝖋𝖋 (flip Op2) expr operator expr
    app = 𝖋 (foldl1 App) (P.many1 expr)
    let_ x arg body = App (Lam x body) arg
    lx =
        Tn.makeTokenParser
            emptyDef
                { Tn.commentStart = "/*"
                , Tn.commentEnd = "*/"
                , Tn.commentLine = "//"
                , Tn.identLetter = Tn.identLetter emptyDef A.<|> P.oneOf "-"
                , Tn.opStart = P.oneOf "+-/*"
                , Tn.opLetter = P.oneOf "+-/*"
                , Tn.reservedNames = ["let", "in", "if", "then", "else", ";"]
                }
    kw = Tn.reserved lx
    try' = P.try ∘ Tn.symbol lx
    ws = Tn.whiteSpace lx
    parens = Tn.parens lx
    iden = Tn.identifier lx
    dot = Tn.dot lx
    nat = Tn.natural lx
    operator = Tn.operator lx
    stringLiteral = Tn.stringLiteral lx

subexprs ∷ (A.Alternative f) ⇒ Expr → f Expr
subexprs =
    A.asum ∘ \case
        Lam _ e → [𝖕 e] <> [subexprs e]
        Num _ → []
        Txt _ → []
        App fn x → map 𝖕 [fn, x] <> [subexprs fn, subexprs x]
        Op2 _ l r → map 𝖕 [l, r] <> [subexprs l, subexprs r]
        If b tru fls → map 𝖕 [b, tru, fls] <> [subexprs b, subexprs tru, subexprs fls]
        Rec _ e → [𝖕 e] <> [subexprs e]
        Vbl _ → []

pruneEnv ∷ ∀ l. Expr → Env l → Env l
pruneEnv e = coerce >>> filter (flip elem (vbls e) ∘ fst) >>> coerce @[(Var, l)] @(Env l)

vbls ∷ Expr → [Var]
vbls e = do Vbl v ← subexprs e; 𝖕 v

instance (Show l) ⇒ F.Fmt (Env l) where
    ansiFmt (Env r) = F.yellow F.>+ "Γ⟦" <> fmt' r "" <> F.yellow F.>+ "⟧"
      where
        fmt' ((v, a) : r') sep = F.start (sep <> v <> ": ") <> F.yellow F.>+ show a <> fmt' r' ", "
        fmt' [] _ = F.start ""

instance (Show l) ⇒ F.Fmt (Store l) where
    ansiFmt =
        coerce @(Store l) @[(l, Val l)] >>> L.reverse >>> \case
            [] → mempty
            (x : xs) → line x <> mconcat (map (line >>> (F.start "\n" <>)) xs)
      where
        prefix = (F.dim F.>+) ∘ \case LamV{} → "lam "; NumV{} → "num "; TxtV{} → "txt "; Undefined{} → "und "
        line (k, v) = F.dim F.>+ "stor " <> F.yellow F.>+ Pr.printf "%-4s" (show k) <> prefix v <> F.ansiFmt v

instance (Show l) ⇒ F.Fmt (Val l) where
    ansiFmt = \case
        LamV x body r → F.start "λ" <> F.bold F.>+ x <> F.start "." <> F.ansiFmt body <> F.start " " <> F.ansiFmt r
        NumV n → F.dim F.>+ show n
        TxtV s → F.dim F.>+ show s
        Undefined s → F.start ("Undefined: " <> s)

instance (Show l) ⇒ F.Fmt (Either String (Val l)) where
    ansiFmt = \case
        Left err → F.start err
        Right val → F.ansiFmt val

instance F.Fmt Expr where
    ansiFmt = \case
        Vbl x → F.start x
        App fn arg → (F.dim <> F.magenta) F.>+ "(" <> appParen fn <> F.start " " <> F.ansiFmt arg <> (F.dim <> F.magenta) F.>+ ")"
        Lam x body → F.dim F.>+ "(λ" <> F.bold F.>+ x <> F.start "." <> binderParen body <> F.dim F.>+ ")"
        Rec f body → F.dim F.>+ "(μ" <> F.bold F.>+ f <> F.start "." <> binderParen body <> F.dim F.>+ ")"
        Op2 o left right → F.dim F.>+ "(" <> F.ansiFmt left <> opFmt o <> F.ansiFmt right <> F.dim F.>+ ")"
        Num n → F.start $ show n
        Txt s → F.dim F.>+ show s
        If etest etrue efalse →
            F.start "(if "
                <> F.ansiFmt etest
                <> F.start " then "
                <> F.ansiFmt etrue
                <> F.start " else "
                <> F.ansiFmt efalse
                <> F.start ")"
      where
        appParen = \case
            App fn arg → appParen fn <> (F.dim <> F.magenta) F.>+ " " <> F.ansiFmt arg
            e → F.ansiFmt e
        binderParen = \case
            Rec f body → F.start "μ" <> F.bold F.>+ f <> F.start "." <> binderParen body
            Lam x body → F.start "λ" <> F.bold F.>+ x <> F.start "." <> binderParen body
            e → F.ansiFmt e
        opFmt o = F.start (" " <> o <> " ")
