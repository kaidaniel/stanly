module Stanly.Parser (parser) where
-- This was first generated by GitHub CoPilot and then corrected by me.

import Text.Parsec
import Stanly.Expr

exprParser :: Parsec String () Expr
exprParser = optional space *> choice [numParser, try ifParser, try lamParser, try recParser, vblParser, try op2Parser, appParser]
  where
    lamParser = parens $ do
      _ <- char 'λ' <|> char '\\'
      x <- many1 letter
      _ <- char '.'
      Lam x <$> exprParser
    recParser = parens $ do
      _ <- string "μ"
      f <- many1 letter
      _ <- char '.'
      Rec f <$> exprParser
    vblParser = Vbl <$> many1 letter
    appParser = parens $ App <$> exprParser <*> exprParser
    op2Parser = parens $ do
      left <- exprParser
      spaces
      op <- char '+' <|> char '-' <|> char '*' <|> char '/'
      spaces
      Op2 [op] left <$> exprParser
    numParser = Num . read <$> many1 digit
    ifParser = parens $ do
      _ <- string "if"
      spaces
      etest <- exprParser
      spaces
      _ <- string "then"
      spaces
      etrue <- exprParser
      spaces
      _ <- string "else"
      spaces
      If etest etrue <$> exprParser

parens :: Parsec String () a -> Parsec String () a
parens p = char '(' *> p <* char ')' <?> "parens"

testExpr :: Expr
testExpr = Lam "x" (App (Vbl "f") (Vbl "x"))

parser :: String -> Either ParseError Expr
parser = parse exprParser ""

failingParse program = case parser program of
  Left err -> error $ show err
  Right expr -> expr

{-|

>>> parser "(x + y)"
Right (x + y)


>>> (failingParse "(f x)") == App (Vbl "f") (Vbl "x")
True


>>> parser "(λx.(f (x + 3)))"
Right (λx.(f (x + 3)))


>>> parser "(λy. (λx.(f x)))"
Right (λy.(λx.(f x)))


|-}