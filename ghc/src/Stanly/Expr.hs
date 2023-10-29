{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Stanly.Expr (Expr (..), Var, parser, parse, expr, strictSubexprs) where
import GHC.Generics
import Text.Parsec
import Stanly.Fmt

type Var = String

data Expr
  = Vbl Var
  | App Expr Expr
  | Lam Var Expr
  | Rec Var Expr
  | Op2 String Expr Expr
  | Num Int
  | If Expr Expr Expr
  deriving (Generic, Eq, Show)

strictSubexprs :: Expr -> [Expr]
strictSubexprs = f 
  where
    f ex = case ex of
      Lam _ e -> e : f e
      Num _ -> []
      App fn x -> fn : x : f fn ++ f x
      Op2 _ l r -> l : r : f l ++ f r
      If b tru fls -> b : tru : fls : f b ++ f tru ++ f fls
      Rec _ e -> e : f e
      Vbl _ -> []

expr :: Parsec String st Expr
expr =
  try ((\x v k -> App (Lam x k) v) <$> "let" `intro` ident <*> "=" `intro` expr <*> "in" `intro` expr)
  <|> between (char '(') (char ')') (ws exprNoParens)
  <|> ws atom
  where
    exprNoParens =
      try (Lam <$> binder ["λ", "fn "] <*> expr)
      </> (Rec <$> binder ["μ", "mu "] <*> expr)
      </> (If <$> "if" `intro` expr <*> "then" `intro` expr <*> "else" `intro` expr)
      </> (App <$> ws expr <*> expr)
      </> (flip Op2 <$> expr <*> op <*> expr)
      <|> atom
    atom =
          (Num <$> integer)
      <|> (Vbl <$> ident)

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

instance Fmt Expr where
  ansiFmt e = case e of
    Vbl x -> green >+ x
    App fn arg -> red >+ "(" <> ansiFmt fn <> red >+ " " <> ansiFmt arg <> red >+ ")"
    Lam x body -> dim >+ "(λ" <> bold >+ x <> start "." <> ansiFmt body <> dim >+ ")"
    Rec f body -> dim >+ "(μ" <> bold >+ f <> start "." <> ansiFmt body <> dim >+ ")"
    Op2 o left right -> dim >+ "(" <> ansiFmt left <> start o <> ansiFmt right <> dim >+ ")"
    Num n -> start $ show n
    If etest etrue efalse -> start "(if " <> ansiFmt etest <> start " then " <> ansiFmt etrue <> start " else " <> ansiFmt efalse <> start ")"
