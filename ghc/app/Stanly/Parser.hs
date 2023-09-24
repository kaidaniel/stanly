module Stanly.Parser (parser, expr, parse) where

import Stanly.Expr
import Text.Parsec
import Text.Read (Lexeme(String))
import Text.Parsec.Token
import GHC.Generics
import Control.Monad.Fix(fix)
import Stanly.Expr (Expr)


var :: Parsec String st String
var  = many1 letter

letExpr :: Parsec String st Expr
letExpr = do
  string "let" >> space; spaces;
  x <- var; spaces;
  string "="; spaces;
  definition <- expr; spaces;
  string "in"; spaces;
  continuation <- expr;
  return (App (Lam x continuation) definition) 
  <?> "let expression [Let]"

expr =  try letExpr
    <|> (between (char '(') (char ')') (spaces *> exprNoParens <* spaces)            <?> "expression [Expr]")
    <|> atom'
  where
    exprNoParens =
          (Lam <$> ((char 'λ' >> spaces) *> var <* (char '.' >> spaces)) <*> expr    <?> "lambda abstraction [Lam]")
      <|> (Rec <$> ((char 'μ' >> spaces) *> var <* (char '.' >> spaces)) <*> expr    <?> "recursive definition [Rec]")
      </> (If  <$> intro "if" <*> intro "then" <*> intro "else"                      <?> "if-then-else expression [If]")
      </> (App <$> (spaces *> expr <* spaces ) <*> expr                              <?> "function application [App]")
      </> do { l <- expr; spaces; o <- arit; spaces; r <- expr; return (Op2 o l r)   <?> "binary operator [Op2]"}
      <|> atom
    atom =
          (Num <$> (read <$> many1 digit)                                            <?> "number [Num]")
      <|> (Vbl <$> var                                                               <?> "variable [Vbl]")
    atom' = spaces *> atom <* spaces
    arit  = choice [string x | x <- ["+", "-", "*", "/"]]
    (</>) l r = l <|> try r
    intro w = do { string w; lookAhead (char '(') <|> space; spaces; e <- expr; spaces; return e;}



-- | Turn program text into an AST.
parser :: String -> Either ParseError Expr
parser = parse (do { ast <- expr; eof; return ast }) "<string>"


-- | Invalid programs.
-- >>> unparse <$> parser "(ifx theny elsez)"
-- >>> unparse <$> parser "(if(λs.1)then2else3)"
-- >>> unparse <$> parser "(if(λs.1)thenABCelseDEF)"
-- Left "<string>" (line 1, column 12):
-- unexpected "e"
-- expecting space, white space or ")"
-- Left "<string>" (line 1, column 10):
-- unexpected "t"
-- expecting white space or ")"
-- Left "<string>" (line 1, column 10):
-- unexpected "t"
-- expecting white space or ")"

-- | Valid programs.
-- >>> unparse <$> parser "(if(λs.1)then(ifxthenyelsez)else(2))"
-- >>> unparse <$> parser "((f)    (a))"
-- >>> unparse <$> parser "(f(a))"
-- >>> unparse <$> parser "(if ifx then theny else (if thenx then elsey else ifz))"
-- >>> unparse <$> parser "(      if x then     y    else   z     )"
-- >>> unparse <$> parser "(x)"
-- >>> unparse <$> parser "let x = (μ f. (f x)) in z"
-- >>> unparse <$> parser "( λ x. ((x) (1)) )"
-- Right "(if (\955s.1) then ifxthenyelsez else 2)"
-- Right "(f a)"
-- Right "(f a)"
-- Right "(if ifx then theny else (if thenx then elsey else ifz))"
-- Right "(if x then y else z)"
-- Right "x"
-- Right "((\955x.z) (\956f.(f x)))"
-- Right "(\955x.(x 1))"
