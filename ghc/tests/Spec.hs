{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- import Stanly.Abstract (execPowerSet)
import Stanly.Fmt (fmt)
import Stanly.Interpreter (Expr (..), parser)
import Stanly.Unicode
import Test.Hspec
import Test.QuickCheck
import Text.Parsec.Error qualified

parser' ∷ String → Either Text.Parsec.Error.ParseError Expr
parser' = parser "<test>"

instance Arbitrary Expr where
    arbitrary = sized \case
        0 → 𝖕 (Num 1)
        n →
            oneof
                [ fmap Num (choose (0, 1000))
                , fmap Txt char
                , fmap Vbl word
                , fmap Op2 operator ⊛ half ⊛ half
                , fmap App half ⊛ half
                , fmap Lam word ⊛ half
                , fmap Rec word ⊛ half
                , fmap If half ⊛ half ⊛ half
                ]
          where
            half = resize (div n 2) arbitrary
            word = oneof (suchThat char (∉ kw) : [𝖕 (x ++ "x") | x ← kw])
            char = listOf1 (elements ['a' .. 'z'])
            operator = elements ["+", "-", "*", "/"]
            kw = ["let", "fn", "rec", "if", "then", "else", "mu", "in"]

shrink ∷ Expr → [Expr]
shrink = genericShrink

main ∷ IO ()
main = hspec $ do
    describe "parser" $ do
        it "parses some edge cases" $ do
            fmap fmt (parser' "((f)    (a))") `shouldBe` Right "(f a)"
            fmap fmt (parser' "(if ifx then theny else (if thenx then elsey else ifz))") `shouldBe` Right "(if ifx then theny else (if thenx then elsey else ifz))"
            fmap fmt (parser' "let x = (μ f. (f x)) in z") `shouldBe` Right "((\955x.z) (\956f.(f x)))"
            fmap fmt (parser' "(mu f.((((((f a) b) c) d) e) f))") `shouldBe` Right "(\956f.(f a b c d e f))"
            fmap fmt (parser' "(fn x.x)") `shouldBe` Right "(\955x.x)"

        it "is inverted by fmt" $
            property \(e ∷ Expr) → fmap fmt (parser' (fmt e)) === Right (fmt e)

--     describe "Abstract.execPowerSet" $ do
--         it "is correct for a few simple examples" $ do
--             resultOf execPowerSet "((3 + 4) * 9)" `shouldMatchList'` ["(Undefined: Top: op2 on Numbers, )"]
--             resultOf execPowerSet "(5 / (1 + 2))" `shouldMatchList'` ["(Undefined: Top: op2 on Numbers, )", "(Undefined: Bottom: Division by zero, )"]
--             resultOf execPowerSet "(if (1 + 0) then 3 else 4)" `shouldMatchList'` ["(4, )", "(3, )"]
--   where
--     resultOf exec' str = fmt (exec' $ either (error . show) id (parser' str))
--     shouldMatchList' = shouldMatchList . lines
