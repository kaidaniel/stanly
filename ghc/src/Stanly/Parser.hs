module Stanly.Parser (parser) where

import Text.Parsec qualified as P
import Text.Parsec.Language (emptyDef)
import Text.Parsec.Token qualified as Tn

import Stanly.Fmt (bwText)
import Stanly.Language (Expr (..), Op2 (..))
import Stanly.Unicode

parser ∷ String → String → Either P.ParseError Expr
parser = P.parse ⎴ ws *> expr <* P.eof

expr ∷ P.Parsec String u Expr
expr = withParens P.<|> withoutParens
  where
    withParens =
        Tn.parens lexer
            ⎴ P.try (ω (flip Op2) ⊛ expr ⊛ operator ⊛ expr)
            P.<|> ω (foldl1 App) ⊛ P.many1 expr
            P.<|> expr
    withoutParens =
        let let' x arg body = App (Lam x body) arg
         in ω Txt ⊛ stringLiteral
                P.<|> ω Lam ⊛ (try₁ "λ" ⫶ try₁ "fn " ≫ iden) ⊛ (dot ≫ expr)
                P.<|> ω Rec ⊛ (try₁ "μ" ⫶ try₁ "mu " ≫ iden) ⊛ (dot ≫ expr)
                P.<|> ω If ⊛ (kw "if" ≫ expr) ⊛ (kw "then" ≫ expr) ⊛ (kw "else" ≫ expr)
                P.<|> ω let' ⊛ (kw "let" ≫ iden) ⊛ (kw "=" ≫ expr) ⊛ (kw "in" ⫶ kw ";" ≫ expr)
                P.<|> ω Num ⊛ nat
                P.<|> ω Var ⊛ iden

operator ∷ P.Parsec String u Op2
operator =
    Tn.operator lexer ⇉ \case
        s
            | s == bwText Plus → ω Plus
            | s == bwText Minus → ω Minus
            | s == bwText Times → ω Times
            | s == bwText Divide → ω Divide
            | otherwise → fail "Invalid operator"

dot, stringLiteral, iden ∷ P.Parsec String u String
dot = Tn.dot lexer
stringLiteral = Tn.stringLiteral lexer
iden = Tn.identifier lexer

ws ∷ P.Parsec String u ()
ws = Tn.whiteSpace lexer

nat ∷ P.Parsec String u Integer
nat = Tn.natural lexer

kw ∷ String → P.Parsec String u ()
kw = Tn.reserved lexer

try₁ ∷ String → P.Parsec String u String
try₁ = P.try ∘ Tn.symbol lexer

lexer ∷ Tn.TokenParser u
lexer =
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
