{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

import GHC.Generics (Generic)
import Stanly.Concrete (execConcrete, execTrace, execNotCovered)
import Stanly.Abstract (execPowerSet)
import Stanly.Fmt (fmt)
import Stanly.Interpreter (Expr (..), parser)
import Test.Hspec
import Test.QuickCheck
import qualified Text.Parsec.Error

parser' :: String -> Either Text.Parsec.Error.ParseError Expr
parser' = parser "<test>"

newtype TestExpr = TestExpr {unTestExpr :: Expr}
  deriving (Generic, Eq, Show)

instance Arbitrary TestExpr where
  arbitrary = sized (fmap TestExpr . arbitrary')
    where
      word :: Gen String
      word = oneof (char : almost_kw)
      almost_kw = [pure (x ++ "x") | x <- kw]
      char = suchThat (listOf1 $ elements ['a' .. 'z']) (`notElem` kw)
      kw = ["let", "fn", "rec", "if", "then", "else", "mu", "in"]
      arbitrary' 0 = pure $ Num 1
      arbitrary' n =
        let rec' = resize (n `div` 2) (fmap unTestExpr arbitrary)
         in oneof
              [ Num <$> choose (0, 1000),
                Txt <$> listOf1 (elements ['a' .. 'z']),
                Vbl <$> word,
                Op2 <$> elements ["+", "-", "*", "/"] <*> rec' <*> rec',
                App <$> rec' <*> rec',
                Lam <$> word <*> rec',
                Rec <$> word <*> rec',
                If <$> rec' <*> rec' <*> rec'
              ]




instance Arbitrary Expr where
  arbitrary = unTestExpr <$> arbitrary

shrink :: TestExpr -> [TestExpr]
shrink = genericShrink

main :: IO ()
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
        \(e :: Expr) -> (fmt <$> (parser' . fmt) e) === Right (fmt e)

  describe "Concrete.execConcrete" $ do
    it "can't apply numbers" $ do
      resultOf execConcrete "(0 1)" `shouldBe` "(Exception: Left hand side of application not bound to a function.\n\nIn function position >>> 0\nIn argument position >>> 1, )"
      resultOf execConcrete "let x = 1 in (x 0)" `shouldBe` "(Exception: Left hand side of application not bound to a function.\n\nIn function position >>> x\nIn argument position >>> 0, 0   num 1)"
      resultOf execConcrete "let f = (fn x.(1 + x)) in ((f 2) 4)" `shouldBe` "(Exception: Left hand side of application not bound to a function.\n\nIn function position >>> (f 2)\nIn argument position >>> 4, 1   num 2\n0   lam λx.(1 + x) Γ⟦⟧)"
    it "can't use undefined variables" $ do
      resultOf execConcrete "(x + 1)" `shouldBe` "(Exception: \"x\" not found in environment: Γ⟦⟧, )"
      resultOf execConcrete "let f = (fn y.(y + 1)) in (f z)" `shouldBe` "(Exception: \"z\" not found in environment: Γ⟦f: 0⟧, 0   lam λy.(y + 1) Γ⟦⟧)"
    it "can't divide by zero" $ do
      resultOf execConcrete "(1 / 0)" `shouldBe` "(Exception: Division by zero. 1/0, )"
      resultOf execConcrete "let x = 2 in let y = 0 in (x / y)" `shouldBe` "(Exception: Division by zero. 2/0, 1   num 0\n0   num 2)"
    it "correctly shadows bindings" $ do
      resultOf execConcrete "let x = 1 in let x = 2 in x" `shouldBe` "(2, 1   num 2\n0   num 1)"
      resultOf execConcrete "let myvar = 1 in ((fn myvar.(myvar + myvar)) 3)" `shouldBe` "(6, 1   num 3\n0   num 1)"

  describe "Concrete.execTrace" $ do
    it "is correct for a few simple examples" $ do
      resultOf execTrace "((3 + 4) * 9)" `shouldBe'` [
        "1. (((3 + 4) * 9), Γ⟦⟧, )",
        "2. ((3 + 4), Γ⟦⟧, )",
        "3. (3, Γ⟦⟧, )",
        "4. (4, Γ⟦⟧, )",
        "5. (9, Γ⟦⟧, )"]
      resultOf execTrace "((fn x.((x + 4) * 9)) 3)" `shouldBe'` [
        "1. (((λx.((x + 4) * 9)) 3), Γ⟦⟧, )",
        "2. ((λx.((x + 4) * 9)), Γ⟦⟧, )",
        "3. (3, Γ⟦⟧, )",
        "4. (((x + 4) * 9), Γ⟦x: 0⟧, 0   num 3)",
        "5. ((x + 4), Γ⟦x: 0⟧, 0   num 3)",
        "6. (x, Γ⟦x: 0⟧, 0   num 3)",
        "7. (4, Γ⟦x: 0⟧, 0   num 3)",
        "8. (9, Γ⟦x: 0⟧, 0   num 3)"]
    it "stops on encountering Bottom" $ do
      resultOf execTrace "((1 / 0) + 5)" `shouldBe'` [
        "1. (((1 / 0) + 5), Γ⟦⟧, )",
        "2. ((1 / 0), Γ⟦⟧, )",
        "3. (1, Γ⟦⟧, )",
        "4. (0, Γ⟦⟧, )"]
      resultOf execTrace "(f x)" `shouldBe'` [
        "1. ((f x), Γ⟦⟧, )",
        "2. (f, Γ⟦⟧, )"]

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
    shouldBe' a b = shouldBe a (unlines b)
    shouldMatchList' = shouldMatchList . lines



-- | Invalid programs.
-- >>> import Stanly.Fmt(fmt)
-- >>> fmt <$> parser' "(ifx theny elsez)"
-- >>> fmt <$> parser' "(if(λs.1)then2else3)"
-- >>> fmt <$> parser' "(if(λs.1)thenABCelseDEF)"
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
-- >>> fmt <$> parser' "(if(λs.1)then(ifxthenyelsez)else(2))"
-- >>> fmt <$> parser' "((f)    (a))"
-- >>> fmt <$> parser' "(f(a))"
-- >>> fmt <$> parser' "(if ifx then theny else (if thenx then elsey else ifz))"
-- >>> fmt <$> parser' "(      if x then     y    else   z     )"
-- >>> fmt <$> parser' "(x)"
-- >>> fmt <$> parser' "let x = (μ f. (f x)) in z"
-- >>> fmt <$> parser' "( λ x. ((x) (1)) )"
-- >>> fmt <$> parser' "(fn x.x)"
-- >>> fmt <$> parser' "(mu f.((f f) 3))"
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
