module Stanly.Parser (parser) where

import Stanly.Expr(Expr(..), Var)
import Text.Parsec
import Text.Read (Lexeme(String))
import Text.Parsec.Token
import GHC.Generics
import Control.Monad.Fix(fix)


var :: Parsec String st String
var  = many1 letter

expr :: Parsec String st Expr
expr =
        (between (char '(') (char ')') (spaces *> exprNoParens <* spaces)            <?> "expression [Expr]")
    <|> atom'
  where
    exprNoParens =
          (Lam <$> (char 'λ' *> var <* char '.') <*> expr                            <?> "lambda abstraction [Lam]")
      <|> (Rec <$> (char 'μ' *> var <* char '.') <*> expr                            <?> "recursive definition [Rec]")
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


parser :: String -> Either ParseError Expr
parser = parse (do { ast <- expr; eof; return ast }) "<string>"
-- ^ Turn program text into an AST.

-- | Invalid programs (edge cases):
-- >>> parser "(ifx theny elsez)"
-- Left "<string>" (line 1, column 12):
-- unexpected "e"
-- expecting space, white space or ")"
-- >>> parser "(if(λs.1)then2else3)"
-- Left "<string>" (line 1, column 10):
-- unexpected "t"
-- expecting white space or ")"
-- >>> parser "(if(λs.1)thenABCelseDEF)"
-- Left "<string>" (line 1, column 10):
-- unexpected "t"
-- expecting white space or ")"
-- >>> parser "( λx.((x) (1)) )"
-- Right (λx.(x 1))

-- | Valid programs (edge cases):
-- >>> parser "(if(λs.1)then(ifxthenyelsez)else(2))"
-- Right (if (λs.1) then ifxthenyelsez else 2)
-- >>> parser "((f)    (a))"
-- Right (f a)
-- >>> parser "(f(a))"
-- Right (f a)
-- >>> parser "(if ifx then theny else (if thenx then elsey else ifz))"
-- Right (if ifx then theny else (if thenx then elsey else ifz))
-- >>> parser "(      if x then     y    else   z     )"
-- Right (if x then y else z)
-- >>> parser "(x)"
-- Right x
-- >>> parser "let x = f y in z"
-- Right let
