module Stanly.Parser (parser) where

import Stanly.Expr(Expr(..), Var)
import Text.Parsec
import Text.Read (Lexeme(String))
import Text.Parsec.Token

expr :: Parsec String st Expr
expr =
        (between (char '(') (char ')') expr'                                                   <?> "expression [Expr]")
    <|> atom
  where
    expr' =
          (Lam <$> (char 'λ' *> var <* char '.') <*> expr                                      <?> "lambda abstraction [Lam]")
      <|> (Rec <$> (char 'μ' *> var <* char '.') <*> expr                                      <?> "recursive definition [Rec]")
      </> (If  <$> intro "if" <*> (space *> intro "then") <*> (space *> intro "else")          <?> "if-then-else expression [If]")
      </> (App <$> (expr <* space ) <*> expr                                                   <?> "function application [App]")
      <|> do { l <- expr; spaces; o <- arit; spaces; r <- expr; return (Op2 o l r)             <?> "binary operator [Op2]"}
      </> atom
    atom =
          (Num <$> (read <$> many1 digit)                                                      <?> "number [Num]")
      <|> (Vbl <$> var                                                                         <?> "variable [Vbl]")
    var  = many1 letter
    arit  = choice [string x | x <- ["+", "-", "*", "/"]]
    (</>) l r = l <|> try r
    intro :: String  -> Parsec String st Expr
    intro w = string w >> spaces *> expr

parser :: String -> Either ParseError Expr
parser = parse expr ""
