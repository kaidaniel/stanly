{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}

module Stanly.Interpreter where

import Control.Applicative qualified as A
import Control.Monad qualified as M
import Control.Monad.Fix (fix)
import Control.Monad.Trans (MonadTrans)
import Control.Monad.Trans.Class (lift)
import Data.Coerce (coerce)
import Data.Function ((&))
import Data.List qualified as L
import GHC.Generics
import Stanly.Fmt (Fmt (..), bold, bwText, dim, magenta, yellow, (⊹))
import Stanly.Unicode
import Text.Parsec qualified as P
import Text.Parsec.Language (emptyDef)
import Text.Parsec.Token qualified as Tn

type Var = String

newtype Env l where
    Env ∷ [(Var, l)] → Env l
    deriving (Eq, Foldable, Semigroup, Monoid)

newtype Store l where
    Store ∷ [(l, Val l)] → Store l
    deriving (Foldable, Semigroup, Monoid)

data Expr where
    Vbl ∷ Var → Expr
    App ∷ Expr → Expr → Expr
    Lam ∷ Var → Expr → Expr
    Rec ∷ Var → Expr → Expr
    Op2 ∷ String → Expr → Expr → Expr
    Num ∷ Integer → Expr
    Txt ∷ String → Expr
    If ∷ Expr → Expr → Expr → Expr
    deriving (Eq, Show, Generic)

data Val l where
    LamV ∷ (Fmt l) ⇒ Var → Expr → Env l → Val l
    NumV ∷ Integer → Val l
    TxtV ∷ String → Val l
    Undefined ∷ String → Val l

deriving instance (Eq l) ⇒ Eq (Val l)
deriving instance Foldable Val

type Eval l m = Expr → m (Val l)
type EvalTr l m = Eval l m → Eval l m
type Combinator l m = Interpreter l m → Expr → m (Val l)

interpret ∷ ∀ l m. Interpreter l m → EvalTr l m → (EvalTr l m → EvalTr l m) → Expr → m (Val l)
interpret interpreter closed open = closed ⎴ fix ⎴ open ⎴ eval interpreter

eval ∷ ∀ m l. Interpreter l m → (Eval l m → Eval l m)
eval Interpreter{..} eval₁ = \case
    Num n → ω (NumV n)
    Txt s → ω (TxtV s)
    Lam v e → ω (LamV v e) ⊛ env
    Vbl vbl → search vbl deref exc
    If b tru fls → branch (eval₁ fls) (eval₁ tru) =<< eval₁ b
    Op2 o e₁ e₂ → M.join ⎴ ω (op2 o) ⊛ eval₁ e₁ ⊛ eval₁ e₂
    Rec f t → do
        l ← alloc f
        resv ← localEnv₁ ([(f, l)] ⋄) ⎴ eval₁ t
        updateStore₁ ([(l, resv)] ⋄)
        ω resv
    App lamV arg →
        eval₁ lamV ⇉ \case
            LamV x body r → do
                evalArg ← eval₁ arg
                allocX ← alloc x
                updateStore₁ ([(allocX, evalArg)] ⋄)
                localEnv (const (coerce [(x, allocX)] ⋄ r)) ⎴ eval₁ body
            _ → exc ⎴ notAFunction lamV arg
  where
    notAFunction lamV arg =
        "Left hand side of application not bound to a function."
            ⋄ "\n\nIn function position ⋙ "
            ⋄ bwText lamV
            ⋄ "\nIn argument position ⋙ "
            ⋄ bwText arg
    search variable iffound ifnotfound =
        env ⇉ \r →
            lookup variable (coerce r) & \case
                Just l → iffound l
                _ → ifnotfound (show variable ⋄ " not found in environment: " ⋄ bwText r)
    localEnv₁ f = localEnv ⎴ coerce f
    updateStore₁ f = updateStore ⎴ coerce f

data Interpreter l m where
    Interpreter ∷
        (Fmt l, Monad m) ⇒
        { deref ∷ l → m (Val l)
        , env ∷ m (Env l)
        , localEnv ∷ (Env l → Env l) → m (Val l) → m (Val l)
        , store ∷ m (Store l)
        , updateStore ∷ (Store l → Store l) → m ()
        , alloc ∷ Var → m l
        , op2 ∷ String → Val l → Val l → m (Val l)
        , branch ∷ m (Val l) → m (Val l) → Val l → m (Val l)
        , exc ∷ String → m (Val l)
        } →
        Interpreter l m

liftInterpreter ∷ ∀ l m n. (MonadTrans n, Monad (n m)) ⇒ Interpreter l m → Interpreter l (n m)
liftInterpreter Interpreter{..} =
    Interpreter
        { deref = lift ∘ deref
        , exc = lift ∘ exc
        , env = lift env
        , alloc = lift ∘ alloc
        , localEnv = \g m → m ⇉ lift ∘ localEnv g ∘ ω
        , store = lift store
        , updateStore = lift ∘ updateStore
        , op2 = \o a b → lift ⎴ op2 o a b
        , branch = \m n v → m ⇉ \x → n ⇉ \y → lift ⎴ branch (ω x) (ω y) v
        }

parser ∷ String → String → Either P.ParseError Expr
parser = P.parse ⎴ expr <* P.eof
  where
    expr = ws *> expr₁
    expr₁ =
        parens (P.try op2₁ <⫶> P.try app <⫶> expr)
            P.<|> ω Txt ⊛ stringLiteral
            P.<|> ω Lam ⊛ (try₁ "λ" <⫶> try₁ "fn " ≫ iden) ⊛ (dot ≫ expr)
            P.<|> ω Rec ⊛ (try₁ "μ" <⫶> try₁ "mu " ≫ iden) ⊛ (dot ≫ expr)
            P.<|> ω If ⊛ (kw "if" ≫ expr) ⊛ (kw "then" ≫ expr) ⊛ (kw "else" ≫ expr)
            P.<|> ω let_ ⊛ (kw "let" ≫ iden) ⊛ (kw "=" ≫ expr) ⊛ (kw "in" <⫶> kw ";" ≫ expr)
            P.<|> ω Num ⊛ nat
            P.<|> ω Vbl ⊛ iden
    op2₁ = ω (flip Op2) ⊛ expr ⊛ operator ⊛ expr
    app = ω (foldl1 App) ⊛ P.many1 expr
    let_ x arg body = App (Lam x body) arg
    lx =
        Tn.makeTokenParser
            emptyDef
                { Tn.commentStart = "/*"
                , Tn.commentEnd = "*/"
                , Tn.commentLine = "//"
                , Tn.identLetter = Tn.identLetter emptyDef ⫶ P.oneOf "-"
                , Tn.opStart = P.oneOf "+-/*"
                , Tn.opLetter = P.oneOf "+-/*"
                , Tn.reservedNames = ["let", "in", "if", "then", "else", ";"]
                }
    kw = Tn.reserved lx
    try₁ = P.try ∘ Tn.symbol lx
    ws = Tn.whiteSpace lx
    parens = Tn.parens lx
    iden = Tn.identifier lx
    dot = Tn.dot lx
    nat = Tn.natural lx
    operator = Tn.operator lx
    stringLiteral = Tn.stringLiteral lx
    (<⫶>) = (P.<|>)
    infixl 2 <⫶>

subexprs ∷ (A.Alternative f) ⇒ Expr → f Expr
subexprs = \case
    Lam _ e → ω e ⫶ subexprs e
    Num _ → εₐ
    Txt _ → εₐ
    App f x → ω f ⫶ ω x ⫶ subexprs f ⫶ subexprs x
    Op2 _ l r → ω l ⫶ ω r ⫶ subexprs l ⫶ subexprs r
    If b t f → ω b ⫶ ω t ⫶ ω f ⫶ subexprs b ⫶ subexprs t ⫶ subexprs f
    Rec _ e → ω e ⫶ subexprs e
    Vbl _ → εₐ

pruneEnv ∷ ∀ l. Expr → Env l → Env l
pruneEnv e = coerce ⋙ filter (flip elem (vbls e) ∘ π₁) ⋙ coerce @[(Var, l)]

vbls ∷ Expr → [Var]
vbls e = do Vbl v ← subexprs e; ω v

instance (Fmt l) ⇒ Fmt (Env l) where
    fmt (Env r) = yellow ⊹ "Γ⟦" ⊹ bwText₁ r "" ⊹ yellow ⊹ "⟧"
      where
        bwText₁ ((v, a) : r₁) sep = sep ⊹ v ⊹ ": " ⊹ yellow ⊹ a ⊹ bwText₁ r₁ ", "
        bwText₁ [] _ = fmt ""

instance (Fmt l) ⇒ Fmt (Store l) where
    fmt =
        coerce @_ @[(l, Val l)] ⋙ L.reverse ⋙ \case
            [] → ε₁
            (x : xs) → line x ⊹ ["\n" ⊹ line x₁ | x₁ ← xs]
      where
        prefix = (dim ⊹) ∘ \case LamV{} → "lam "; NumV{} → "num "; TxtV{} → "txt "; Undefined{} → "und "
        line (k, v) = dim ⊹ "stor " ⊹ yellow ⊹ take 4 (bwText k) ⊹ prefix v ⊹ v

instance (Fmt l) ⇒ Fmt (Val l) where
    fmt = \case
        LamV x body r → "λ" ⊹ bold ⊹ x ⊹ "." ⊹ body ⊹ " " ⊹ r
        NumV n → dim ⊹ n
        TxtV s → dim ⊹ s
        Undefined s → "Undefined: " ⊹ s

instance (Fmt l) ⇒ Fmt (Either String (Val l)) where
    fmt = \case
        Left err → fmt err
        Right val → fmt val

instance Fmt Expr where
    fmt = \case
        Vbl x → fmt x
        App f x → (dim ⋄ magenta) ⊹ "(" ⊹ paren₁ f ⊹ " " ⊹ x ⊹ (dim ⋄ magenta) ⊹ ")"
        Lam x fn → dim ⊹ "(λ" ⊹ bold ⊹ x ⊹ "." ⊹ paren₂ fn ⊹ dim ⊹ ")"
        Rec x fn → dim ⊹ "(μ" ⊹ bold ⊹ x ⊹ "." ⊹ paren₂ fn ⊹ dim ⊹ ")"
        Op2 o e₁ e₂ → dim ⊹ "(" ⊹ e₁ ⊹ " " ⊹ o ⊹ " " ⊹ e₂ ⊹ dim ⊹ ")"
        Num n → fmt n
        Txt s → dim ⊹ show s
        If tst e₁ e₂ → "(if " ⊹ tst ⊹ " then " ⊹ e₁ ⊹ " else " ⊹ e₂ ⊹ ")"
      where
        paren₁ = \case App f x → paren₁ f ⊹ " " ⊹ x; e → fmt e
        paren₂ = \case Rec x fn → k "μ" x fn; Lam x fn → k "λ" x fn; e → fmt e
        k sym x fn = sym ⊹ bold ⊹ x ⊹ "." ⊹ paren₂ fn
