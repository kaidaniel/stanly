{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- import Stanly.Abstract (execPowerSet)
import Stanly.Fmt (bwText)
import Stanly.Interpreter (Expr (..), parser)
import Stanly.Unicode
import Test.Hspec
import Test.QuickCheck
import Text.Parsec.Error qualified

parser₁ ∷ String → Either Text.Parsec.Error.ParseError Expr
parser₁ = parser "<test>"

instance Arbitrary Expr where
    arbitrary = sized \case
        0 → ω (Num 1)
        n →
            oneof
                [ φ Num (choose (0, 1000))
                , φ Txt char
                , φ Vbl word
                , φ Op2 operator ⊛ half ⊛ half
                , φ App half ⊛ half
                , φ Lam word ⊛ half
                , φ Rec word ⊛ half
                , φ If half ⊛ half ⊛ half
                ]
          where
            half = resize (div n 2) arbitrary
            word = oneof (suchThat char (`notElem` kw) : [ω (x ++ "x") | x ← kw])
            char = listOf1 (elements ['a' .. 'z'])
            operator = elements ["+", "-", "*", "/"]
            kw = ["let", "fn", "rec", "if", "then", "else", "mu", "in"]

shrink ∷ Expr → [Expr]
shrink = genericShrink

main ∷ IO ()
main =
    hspec ⎴ do
        describe "parser" ⎴ do
            it "parses some edge cases" ⎴ do
                φ bwText (parser₁ "((f)    (a))") `shouldBe` Right "(f a)"
                φ bwText (parser₁ "(if ifx then theny else (if thenx then elsey else ifz))") `shouldBe` Right "(if ifx then theny else (if thenx then elsey else ifz))"
                φ bwText (parser₁ "let x = (μ f. (f x)) in z") `shouldBe` Right "((\955x.z) (\956f.(f x)))"
                φ bwText (parser₁ "(mu f.((((((f a) b) c) d) e) f))") `shouldBe` Right "(\956f.(f a b c d e f))"
                φ bwText (parser₁ "(fn x.x)") `shouldBe` Right "(\955x.x)"

            it "is inverted by bwText"
                ⎴ property \(e ∷ Expr) → φ bwText (parser₁ (bwText e)) === Right (bwText e)

--     describe "Abstract.execPowerSet" ⎴ do
--         it "is correct for a few simple examples" ⎴ do
--             resultOf execPowerSet "((3 + 4) * 9)" `shouldMatchList₁` ["(Undefined: Top: op₂ on Numbers, )"]
--             resultOf execPowerSet "(5 / (1 + 2))" `shouldMatchList₁` ["(Undefined: Top: op₂ on Numbers, )", "(Undefined: Bottom: Division by zero, )"]
--             resultOf execPowerSet "(if (1 + 0) then 3 else 4)" `shouldMatchList₁` ["(4, )", "(3, )"]
--   where
--     resultOf exec₁ str = bwText (exec₁ ⎴ either (error ∘ show) id (parser₁ str))
--     shouldMatchList₁ = shouldMatchList ∘ lines
