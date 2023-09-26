{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module Stanly.Parser (parser, expr, parse) where

import Stanly.Expr(Fmt(..), Expr(..))
import Text.Parsec
import Stanly.IdiomBrackets(i)

expr :: Parsec String st Expr
expr =  
  try [i| (\x v k -> App (Lam x k) v) ("let" `intro` ident) ("=" `intro` expr) ("in" `intro` expr) |]
  <|> between (char '(') (char ')') (ws exprNoParens)
  <|> ws atom
  where
    exprNoParens =
      try [i| Lam (binder ["λ", "fn "]) expr |]
      </> [i| Rec (binder ["μ", "mu "]) expr |]
      </> [i| If ("if" `intro` expr) ("then" `intro` expr) ("else" `intro` expr) |]
      </> [i| App (ws expr) expr |]
      </> [i| (flip Op2) expr op expr |]
      <|> atom
    atom =
          [i| Num integer |]
      <|> [i| Vbl ident |]

    op  = ws (choice [string x | x <- ["+", "-", "*", "/"]])
    binder c = (choice [string x | x <- c]) *> ws (ident <* char '.')
    (</>) l r = l <|> try r
    intro w e = do { string w; lookAhead (char '(') <|> space; ws e}
    integer = read <$> many1 digit
    ws e = spaces *> e <* spaces
    ident  = many1 letter


-- | Turn program text into an AST.
parser :: String -> Either ParseError Expr
parser = parse (expr <* eof) "<string>"


-- | Invalid programs.
-- >>> fmt <$> parser "(ifx theny elsez)"
-- >>> fmt <$> parser "(if(λs.1)then2else3)"
-- >>> fmt <$> parser "(if(λs.1)thenABCelseDEF)"
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
-- >>> fmt <$> parser "(if(λs.1)then(ifxthenyelsez)else(2))"
-- >>> fmt <$> parser "((f)    (a))"
-- >>> fmt <$> parser "(f(a))"
-- >>> fmt <$> parser "(if ifx then theny else (if thenx then elsey else ifz))"
-- >>> fmt <$> parser "(      if x then     y    else   z     )"
-- >>> fmt <$> parser "(x)"
-- >>> fmt <$> parser "let x = (μ f. (f x)) in z"
-- >>> fmt <$> parser "( λ x. ((x) (1)) )"
-- >>> fmt <$> parser "(fn x.x)"
-- >>> fmt <$> parser "(mu f.((f f) 3))"
-- Right "(if (\955s.1) then ifxthenyelsez else 2)"
-- Right "(f a)"
-- Right "(f a)"
-- Right "(if ifx then theny else (if thenx then elsey else ifz))"
-- Right "(if x then y else z)"
-- Right "x"
-- Right "((\955x.z) (\956f.(f x)))"
-- Right "(\955x.(x 1))"
-- Right "(\955x.x)"
-- Right "(\956f.((f f) 3))"
