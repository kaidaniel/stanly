{-# LANGUAGE BlockArguments #-}

-- import Stanly.Abstract (execPowerSet)
import Stanly.Fmt (bwText)
import Stanly.Language (Expr)
import Stanly.Parser (parser)
import Stanly.Unicode
import Test.Hspec (describe, hspec, it, shouldBe)
import Test.QuickCheck (Testable (property), (===))

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
  where
    parser₁ = parser "<test>"
