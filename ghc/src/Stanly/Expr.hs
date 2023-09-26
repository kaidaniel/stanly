{-# LANGUAGE DeriveGeneric #-}

module Stanly.Expr (Expr (..), Var, Fmt(..)) where
import GHC.Generics
import Test.QuickCheck

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

class Fmt a where
  fmt :: a -> String

instance Fmt Expr where
  fmt (Vbl x) = x
  fmt (App fn arg) = "(" ++ fmt fn ++ " " ++ fmt arg ++ ")"
  fmt (Lam x body) = "(λ" ++ x ++ "." ++ fmt body ++ ")"
  fmt (Rec f body) = "(μ" ++ f ++ "." ++ fmt body ++ ")"
  fmt (Op2 o left right) = "(" ++ fmt left ++ " " ++ o ++ " " ++ fmt right ++ ")"
  fmt (Num n) = show n
  fmt (If etest etrue efalse) = "(if " ++ fmt etest ++ " then " ++ fmt etrue ++ " else " ++ fmt efalse ++ ")"

instance Arbitrary Expr where
  arbitrary = sized arbitrary'
    where
      word :: Gen String
      word = listOf1 (elements ['a' .. 'z'])
      arbitrary' 0 = pure $ Num 1
      arbitrary' n =
        let rec' = resize (n `div` 2) arbitrary
         in oneof
              [ Num <$> choose (0, 1000),
                Vbl <$> word,
                Op2 <$> elements ["+", "-", "*", "/"] <*> rec' <*> rec',
                App <$> rec' <*> rec',
                Lam <$> word <*> rec',
                Rec <$> word <*> rec',
                If  <$> rec' <*> rec' <*> rec'
              ]

shrink :: Expr -> [Expr]
shrink = genericShrink