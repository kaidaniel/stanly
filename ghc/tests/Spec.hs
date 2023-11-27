{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

import GHC.Generics (Generic)
import Stanly.Abstract (execPowerSet)
import Stanly.Concrete (execNotCovered)
import Stanly.Fmt (fmt)
import Stanly.Interpreter (Expr (..), parser)
import Test.Hspec
import Test.QuickCheck
import Text.Parsec.Error qualified

parser' ∷ String → Either Text.Parsec.Error.ParseError Expr
parser' = parser "<test>"

newtype TestExpr = TestExpr {unTestExpr ∷ Expr}
    deriving (Generic, Eq, Show)

instance Arbitrary TestExpr where
    arbitrary = sized (fmap TestExpr . arbitrary')
      where
        word ∷ Gen String
        word = oneof (char : almost_kw)
        almost_kw = [pure (x ++ "x") | x ← kw]
        char = suchThat (listOf1 $ elements ['a' .. 'z']) (`notElem` kw)
        kw = ["let", "fn", "rec", "if", "then", "else", "mu", "in"]
        arbitrary' 0 = pure $ Num 1
        arbitrary' n =
            let rec' = resize (n `div` 2) (fmap unTestExpr arbitrary)
             in oneof
                    [ Num <$> choose (0, 1000)
                    , Txt <$> listOf1 (elements ['a' .. 'z'])
                    , Vbl <$> word
                    , Op2 <$> elements ["+", "-", "*", "/"] <*> rec' <*> rec'
                    , App <$> rec' <*> rec'
                    , Lam <$> word <*> rec'
                    , Rec <$> word <*> rec'
                    , If <$> rec' <*> rec' <*> rec'
                    ]

instance Arbitrary Expr where
    arbitrary = unTestExpr <$> arbitrary

shrink ∷ TestExpr → [TestExpr]
shrink = genericShrink

main ∷ IO ()
main = hspec $ do
    describe "parser" $ do
        it "parses some edge cases" $ do
            fmt <$> parser' "((f)    (a))" `shouldBe` Right "(f a)"
            fmt <$> parser' "(if ifx then theny else (if thenx then elsey else ifz))" `shouldBe` Right "(if ifx then theny else (if thenx then elsey else ifz))"
            fmt <$> parser' "let x = (μ f. (f x)) in z" `shouldBe` Right "((\955x.z) (\956f.(f x)))"
            fmt <$> parser' "(mu f.((((((f a) b) c) d) e) f))" `shouldBe` Right "(\956f.(f a b c d e f))"
            fmt <$> parser' "(fn x.x)" `shouldBe` Right "(\955x.x)"

        it "is inverted by fmt" $
            property $
                \(e ∷ Expr) → (fmt <$> (parser' . fmt) e) === Right (fmt e)

    describe "Concrete.execNotCovered" $ do
        it "is correct for a few simple examples" $ do
            resultOf execNotCovered "(if 0 then 2 else 1)" `shouldMatchList'` ["2"]
            resultOf execNotCovered "(fn x.(x))" `shouldMatchList'` ["x"]
            resultOf execNotCovered "(if (1 / 0) then 2 else 3)" `shouldMatchList'` ["2", "3"]

    describe "Abstract.execPowerSet" $ do
        it "is correct for a few simple examples" $ do
            resultOf execPowerSet "((3 + 4) * 9)" `shouldMatchList'` ["(Undefined: Top: op2 on Numbers, )"]
            resultOf execPowerSet "(5 / (1 + 2))" `shouldMatchList'` ["(Undefined: Top: op2 on Numbers, )", "(Undefined: Bottom: Division by zero, )"]
            resultOf execPowerSet "(if (1 + 0) then 3 else 4)" `shouldMatchList'` ["(4, )", "(3, )"]
  where
    resultOf exec' str = fmt (exec' $ either (error . show) id (parser' str))
    shouldMatchList' = shouldMatchList . lines
