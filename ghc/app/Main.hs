{-# HLINT ignore "Redundant bracket" #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main where

import Stanly.ConcreteSemantics (Value (..), eval)
import Stanly.Expr (Expr (..))
import Stanly.Parser (parser)
import Test.HUnit
import Test.QuickCheck
  ( Arbitrary (arbitrary),
    Gen,
    choose,
    elements,
    listOf1,
    oneof,
    quickCheck,
    resize,
    sized,
    verboseCheck,
    withMaxSuccess,
  )

instance Arbitrary Expr where
  arbitrary :: Gen Expr
  arbitrary = sized arbitrary'
    where
      word :: Gen String
      word = listOf1 (elements ['a' .. 'z'])
      arbitrary' 0 = pure $ Num 1
      arbitrary' n =
        let rec' = resize (n `div` 2) arbitrary
         in oneof
              [ Num <$> (choose (0, 1000)),
                Vbl <$> word,
                Op2 <$> (elements ["+", "-", "*", "/"]) <*> rec' <*> rec',
                App <$> rec' <*> rec',
                Lam <$> word <*> rec',
                Rec <$> word <*> rec',
                If <$> rec' <*> rec' <*> rec'
              ]

-- runTestTT (TestList [
--   TestList ["parser(show(x)) == x" ~: parser (show e) ~?= Right e | e <- asts],
--   TestList ["show(parser(x)) == x" ~: show (case parser p of Right x -> x; y -> error $ show y) ~?= p | p <- programs]
--   ]) >>

prop_parser_show :: Expr -> Bool
prop_parser_show e = parser (show e) == Right e

main :: IO ()
main =
  verboseCheck (withMaxSuccess 10 prop_parser_show)
    >> quickCheck (withMaxSuccess 1000 prop_parser_show)
