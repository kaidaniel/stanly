{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}

module Stanly.Interpreter where

import Control.Applicative qualified as A
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Fix (fix)
import Control.Monad.Trans (MonadTrans)
import Control.Monad.Trans.Class (lift)
import Data.Coerce (coerce)
import Data.Function ((&))
import Data.List qualified as L
import GHC.Generics
import Stanly.Fmt
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

data Op2 where
    Plus ∷ Op2
    Minus ∷ Op2
    Times ∷ Op2
    Divide ∷ Op2
    deriving (Eq, Show)

data Expr where
    Vbl ∷ Var → Expr
    App ∷ Expr → Expr → Expr
    Lam ∷ Var → Expr → Expr
    Rec ∷ Var → Expr → Expr
    Op2 ∷ Op2 → Expr → Expr → Expr
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
type Combinator l m = Interpreter l m → Eval l m

interpret ∷ ∀ l m. (Eval l m → Eval l m) → ((Eval l m → Eval l m) → (Eval l m → Eval l m)) → Combinator l m
interpret closed open interpreter = closed ⎴ fix ⎴ open ⎴ eval interpreter

eval ∷ ∀ m l. Interpreter l m → Eval l m → Eval l m
eval Interpreter{..} eval₁ = \case
    Num n → ω (NumV n)
    Txt s → ω (TxtV s)
    Lam v e → φ (LamV v e) env
    Vbl vbl → search vbl deref exc
    If tst then' else' → branch (eval₁ tst) (eval₁ then') (eval₁ else')
    Op2 o e₁ e₂ → op2 o (eval₁ e₁) (eval₁ e₂)
    Rec vbl body → do
        loc ← alloc vbl
        value ← localEnv₂ (vbl, loc) body
        updateStore₁ (loc, value)
        ω value
    App lamV arg →
        eval₁ lamV ⇉ \case
            LamV vbl body env₁ → do
                arg₁ ← eval₁ arg
                loc ← alloc vbl
                updateStore₁ (loc, arg₁)
                localEnv₁ env₁ (vbl, loc) body
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
    localEnv₂ binding cc = localEnv (coerce [binding] ⋄) ⎴ eval₁ cc
    localEnv₁ env₁ binding cc = localEnv (const (coerce [binding] ⋄ env₁)) ⎴ eval₁ cc
    updateStore₁ binding = updateStore (coerce [binding] ⋄)

data Interpreter l m where
    Interpreter ∷
        (Fmt l, Monad m) ⇒
        { deref ∷ l → m (Val l)
        , env ∷ m (Env l)
        , localEnv ∷ (Env l → Env l) → m (Val l) → m (Val l)
        , store ∷ m (Store l)
        , updateStore ∷ (Store l → Store l) → m ()
        , alloc ∷ Var → m l
        , op2 ∷ Op2 → m (Val l) → m (Val l) → m (Val l)
        , branch ∷ m (Val l) → m (Val l) → m (Val l) → m (Val l)
        , exc ∷ String → m (Val l)
        } →
        Interpreter l m

liftInterpreter ∷ ∀ l m t. (MonadTrans t, Monad (t m)) ⇒ Interpreter l m → Interpreter l (t m)
liftInterpreter Interpreter{..} =
    Interpreter
        { deref = ζ₀ ∘ deref
        , exc = ζ₀ ∘ exc
        , env = ζ₀ env
        , alloc = ζ₀ ∘ alloc
        , localEnv = ζ₁ ∘ localEnv
        , store = ζ₀ store
        , updateStore = ζ₀ ∘ updateStore
        , op2 = ζ₂ ∘ op2
        , branch = ζ₃ branch
        }
  where
    ζ₀ ∷ m r → t m r
    ζ₁ ∷ (m x₁ → m r) → (t m x₁ → t m r)
    ζ₂ ∷ (m x₁ → m x₂ → m r) → (t m x₁ → t m x₂ → t m r)
    ζ₃ ∷ (m x₁ → m x₂ → m x₃ → m r) → (t m x₁ → t m x₂ → t m x₃ → t m r)

    f ⊰ a = f ⊛ φ ω a
    infixl 4 ⊰
    ζ₀ = lift
    ζ₁ f x = ω f ⊰ x ⇉ ζ₀
    ζ₂ f x y = ω f ⊰ x ⊰ y ⇉ ζ₀
    ζ₃ f x y z = ω f ⊰ x ⊰ y ⊰ z ⇉ ζ₀

parser ∷ String → String → Either P.ParseError Expr
parser = P.parse ⎴ ws *> expr <* P.eof
  where
    expr =
        parens (P.try op2₁ <⫶> P.try app <⫶> expr)
            P.<|> ω Txt ⊛ stringLiteral
            P.<|> ω Lam ⊛ try₁ "λ" <⫶> try₁ "fn " ⫸ iden ⊛ dot ⫸ expr
            P.<|> ω Rec ⊛ try₁ "μ" <⫶> try₁ "mu " ⫸ iden ⊛ dot ⫸ expr
            P.<|> ω If ⊛ kw "if" ⫸ expr ⊛ kw "then" ⫸ expr ⊛ kw "else" ⫸ expr
            P.<|> ω let' ⊛ kw "let" ⫸ iden ⊛ kw "=" ⫸ expr ⊛ kw "in" <⫶> kw ";" ⫸ expr
            P.<|> ω Num ⊛ nat
            P.<|> ω Vbl ⊛ iden
    op2₁ = ω (flip Op2) ⊛ expr ⊛ operator ⊛ expr
    app = ω (foldl1 App) ⊛ P.many1 expr
    let' x arg body = App (Lam x body) arg
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
    operator = Tn.operator lx ⇉ \case "+" → ω Plus; "-" → ω Minus; "*" → ω Times; "/" → ω Divide; o → fail ⎴ "Invalid operator: '" ⋄ o ⋄ "'"
    stringLiteral = Tn.stringLiteral lx
    (<⫶>) = (P.<|>)
    (⫸) = (*>)
    infixl 6 <⫶>
    infixl 5 ⫸

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
pruneEnv e = coerce @[(Var, l)] ∘ filter (flip elem (vbls e) ∘ π₁) ∘ coerce

vbls ∷ Expr → [Var]
vbls e = do Vbl v ← subexprs e; ω v

exception ∷ (MonadError String m) ⇒ String → m a
exception msg = throwError ⎴ "Exception: " ⋄ msg

arithmetic ∷ (Fmt l, MonadError String m) ⇒ Op2 → m (Val l) → m (Val l) → m (Val l)
arithmetic o a b = do
    a₁ ← a
    b₁ ← b
    let exc msg = exception ⎴ bwText ⎴ msg ⊹ ".\nWhen evaluating: " ⊹ a₁ ⊹ o ⊹ b₁
    case (a₁, b₁) of
        (NumV n₀, NumV n₁)
            | o == Plus → ω ⎴ NumV ⎴ n₀ + n₁
            | o == Minus → ω ⎴ NumV ⎴ n₀ - n₁
            | o == Times → ω ⎴ NumV ⎴ n₀ * n₁
            | o == Divide, n₁ == 0 → exc "Division by zero"
            | o == Divide → ω ⎴ NumV ⎴ div n₀ n₁
        (TxtV t₀, TxtV t₁)
            | o == Plus → ω ⎴ TxtV ⎴ t₀ ⋄ t₁
        (TxtV t₀, NumV n₁)
            | o == Plus → ω ⎴ TxtV ⎴ t₀ ⋄ show n₁
        _ → exc "Invalid arguments to operator"

branchIfn0 ∷ (MonadError String m) ⇒ m (Val l) → m (Val l) → m (Val l) → m (Val l)
branchIfn0 tst then' else' =
    tst ⇉ \case
        NumV n | n == 0 → else' | otherwise → then'
        _ → exception "Branching on non-numeric value."

instance Fmt Op2 where
    fmt = fmt ∘ \case Plus → "+"; Minus → "-"; Times → "*"; Divide → "/"

instance (Fmt l) ⇒ Fmt (Env l) where
    fmt (Env r) = (Yellow ⊹ "Γ⟦") ⊹ fmt₁ (r, "") ⊹ (Yellow ⊹ "⟧")
      where
        fmt₁ = \case
            ((v, a) : r₁, sep) → sep ⊹ v ⊹ ": " ⊹ (Yellow ⊹ a) ⊹ fmt₁ (r₁, ", ")
            ([], _) → fmt ""

instance (Fmt l) ⇒ Fmt (Store l) where
    fmt =
        coerce @_ @[(l, Val l)] ⋙ L.reverse ⋙ \case
            [] → ε₁
            (x : xs) → line x ⊹ ["\n" ⊹ line x₁ | x₁ ← xs]
      where
        prefix = (Dim ⊹) ∘ \case LamV{} → "lam "; NumV{} → "num "; TxtV{} → "txt "; Undefined{} → "und "
        line (k, v) = (Dim ⊹ "stor ") ⊹ (Yellow ⊹ (padded ⎴ bwText k) ⊹ " ") ⊹ prefix v ⊹ v
          where
            padded ∷ String → String
            padded = \case
                [] → "    "
                s@[_] → "   " ⋄ s
                s@[_, _] → "  " ⋄ s
                s@[_, _, _] → " " ⋄ s
                s → s

instance (Fmt l) ⇒ Fmt (Val l) where
    fmt = \case
        LamV x body r → "λ" ⊹ (Bold ⊹ x) ⊹ "." ⊹ body ⊹ " " ⊹ r
        NumV n → Dim ⊹ n
        TxtV s → Dim ⊹ s
        Undefined s → "Undefined: " ⊹ s

instance (Fmt l) ⇒ Fmt (Either String (Val l)) where
    fmt = \case
        Left err → fmt err
        Right val → fmt val

instance Fmt Expr where
    fmt = \case
        Vbl x → fmt x
        App f x → (Dim ⊹ Magenta ⊹ "(") ⊹ paren₁ f ⊹ " " ⊹ x ⊹ (Dim ⊹ Magenta ⊹ ")")
        Lam x fn → (Dim ⊹ "(λ") ⊹ (Bold ⊹ x) ⊹ "." ⊹ paren₂ fn ⊹ (Dim ⊹ ")")
        Rec x fn → (Dim ⊹ "(μ") ⊹ (Bold ⊹ x) ⊹ "." ⊹ paren₂ fn ⊹ (Dim ⊹ ")")
        Op2 o e₁ e₂ → (Dim ⊹ "(") ⊹ e₁ ⊹ " " ⊹ o ⊹ " " ⊹ e₂ ⊹ (Dim ⊹ ")")
        Num n → fmt n
        Txt s → (Dim ⊹ show s)
        If tst e₁ e₂ → "(if " ⊹ tst ⊹ " then " ⊹ e₁ ⊹ " else " ⊹ e₂ ⊹ ")"
      where
        paren₁ = \case App f x → paren₁ f ⊹ " " ⊹ x; e → fmt e
        paren₂ = \case Rec x fn → k "μ" x fn; Lam x fn → k "λ" x fn; e → fmt e
        k sym x fn = sym ⊹ (Bold ⊹ x) ⊹ "." ⊹ paren₂ fn
