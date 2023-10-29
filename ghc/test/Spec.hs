{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

import GHC.Generics (Generic)
import Stanly.Concrete (execConcrete, execTrace, execNotCovered)
import Stanly.Abstract (execPowerSet)
import Stanly.Expr (Expr (..), parser)
import Stanly.Fmt (fmt)
import Test.Hspec
import Test.QuickCheck

newtype TestExpr = TestExpr {unTestExpr :: Expr}
  deriving (Generic, Eq, Show)

instance Arbitrary TestExpr where
  arbitrary = sized (fmap TestExpr . arbitrary')
    where
      word :: Gen String
      word = oneof [listOf1 $ elements ['a' .. 'z'], pure "letx", pure "fnx", pure "recx", pure "ifx", pure "thenx", pure "elsex", pure "mux"]
      arbitrary' 0 = pure $ Num 1
      arbitrary' n =
        let rec' = resize (n `div` 2) (fmap unTestExpr arbitrary)
         in oneof
              [ Num <$> choose (0, 1000),
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
      fmt <$> parser "(if(λs.1)then(ifxthenyelsez)else(2))" `shouldBe` Right "(if (\955s.1) then ifxthenyelsez else 2)"
      fmt <$> parser "((f)    (a))" `shouldBe` Right "(f a)"
      fmt <$> parser "(if ifx then theny else (if thenx then elsey else ifz))" `shouldBe` Right "(if ifx then theny else (if thenx then elsey else ifz))"
      fmt <$> parser "let x = (μ f. (f x)) in z" `shouldBe` Right "((\955x.z) (\956f.(f x)))"
      fmt <$> parser "(mu f.((f f) 3))" `shouldBe` Right "(\956f.((f f) 3))"
      fmt <$> parser "(fn x.x)" `shouldBe` Right "(\955x.x)"

    it "is inverted by fmt" $
      property $
        \(e :: Expr) -> (fmt <$> (parser . fmt) e) === Right (fmt e)

  describe "Concrete.execConcrete" $ do
    it "is correct for a few some examples" $ do
      resultOf execConcrete "let x = 1 in ((fn x.(x + x)) 2)" `shouldBe` "(4, Σ⟦1↦2,0↦1⟧)"
      resultOf execConcrete "let x = (1 + 10) in ((λy.(λf.(f x))) (2 + 20))" `shouldBe` "(λf.(f x)⟦y↦1,x↦0⟧, Σ⟦1↦22,0↦11⟧)"
      resultOf execConcrete "((fn g.(fn x.(g x))) 3)" `shouldBe` "(λx.(g x)⟦g↦0⟧, Σ⟦0↦3⟧)"
      resultOf execConcrete "let f = (fn g.(g 0)) in ((fn y.(fn g.y)) f)" `shouldBe` "(λg.y⟦y↦1,f↦0⟧, Σ⟦1↦λg.(g 0)⟦⟧,0↦λg.(g 0)⟦⟧⟧)"
    it "can't apply numbers" $ do
      resultOf execConcrete "(0 1)" `shouldBe` "(Exception: \"0\" is not a function, Σ⟦⟧)"
      resultOf execConcrete "let x = 1 in (x 0)" `shouldBe` "(Exception: \"x\" is not a function, Σ⟦0↦1⟧)"
      resultOf execConcrete "let f = (fn x.(1 + x)) in ((f 2) 4)" `shouldBe` "(Exception: \"(f 2)\" is not a function, Σ⟦1↦2,0↦λx.(1+x)⟦⟧⟧)"
    it "can't use undefined variables" $ do
      resultOf execConcrete "(x + 1)" `shouldBe` "(Exception: \"x\" not found in environment: ⟦⟧, Σ⟦⟧)"
      resultOf execConcrete "let f = (fn y.(y + 1)) in (f z)" `shouldBe` "(Exception: \"z\" not found in environment: ⟦f↦0⟧, Σ⟦0↦λy.(y+1)⟦⟧⟧)"
    it "can't divide by zero" $ do
      resultOf execConcrete "(1 / 0)" `shouldBe` "(Exception: Division by zero. 1/0, Σ⟦⟧)"
      resultOf execConcrete "let x = 2 in let y = 0 in (x / y)" `shouldBe` "(Exception: Division by zero. 2/0, Σ⟦1↦0,0↦2⟧)"
    it "correctly shadows bindings" $ do
      resultOf execConcrete "let x = 1 in let x = 2 in x" `shouldBe` "(2, Σ⟦1↦2,0↦1⟧)"
      resultOf execConcrete "let myvar = 1 in ((fn myvar.(myvar + myvar)) 3)" `shouldBe` "(6, Σ⟦1↦3,0↦1⟧)"

  describe "Concrete.execTrace" $ do
    it "is correct for a few simple examples" $ do
      resultOf execTrace "((3 + 4) * 9)" `shouldBe'` [
        "1. (((3+4)*9), ⟦⟧, Σ⟦⟧)", 
        "2. ((3+4), ⟦⟧, Σ⟦⟧)", 
        "3. (3, ⟦⟧, Σ⟦⟧)", 
        "4. (4, ⟦⟧, Σ⟦⟧)", 
        "5. (9, ⟦⟧, Σ⟦⟧)"]
      resultOf execTrace "((fn x.((x + 4) * 9)) 3)" `shouldBe'` [
        "1. (((λx.((x+4)*9)) 3), ⟦⟧, Σ⟦⟧)",
        "2. ((λx.((x+4)*9)), ⟦⟧, Σ⟦⟧)",
        "3. (3, ⟦⟧, Σ⟦⟧)",
        "4. (((x+4)*9), ⟦x↦0⟧, Σ⟦0↦3⟧)",
        "5. ((x+4), ⟦x↦0⟧, Σ⟦0↦3⟧)",
        "6. (x, ⟦x↦0⟧, Σ⟦0↦3⟧)",
        "7. (4, ⟦x↦0⟧, Σ⟦0↦3⟧)",
        "8. (9, ⟦x↦0⟧, Σ⟦0↦3⟧)"]
    it "stops on encountering Bottom" $ do
      resultOf execTrace "((1 / 0) + 5)" `shouldBe'` [
        "1. (((1/0)+5), ⟦⟧, Σ⟦⟧)",
        "2. ((1/0), ⟦⟧, Σ⟦⟧)",
        "3. (1, ⟦⟧, Σ⟦⟧)",
        "4. (0, ⟦⟧, Σ⟦⟧)"]
      resultOf execTrace "(f x)" `shouldBe'` [
        "1. ((f x), ⟦⟧, Σ⟦⟧)",
        "2. (f, ⟦⟧, Σ⟦⟧)"]

  describe "Concrete.execNotCovered" $ do
    it "is correct for a few simple examples" $ do
      resultOf execNotCovered "(if 0 then 2 else 1)" `shouldMatchList'` ["2"]
      resultOf execNotCovered "(fn x.(x))" `shouldMatchList'` ["x"]
      resultOf execNotCovered "(if (1 / 0) then 2 else 3)" `shouldMatchList'` ["2", "3"]

  describe "Abstract.execPowerSet" $ do
    it "is correct for a few simple examples" $ do
      resultOf execPowerSet "((3 + 4) * 9)" `shouldMatchList'` ["(Undefined: op2 on Numbers, Σ⟦⟧)"]
      resultOf execPowerSet "(5 / (1 + 2))" `shouldMatchList'` ["(Undefined: op2 on Numbers, Σ⟦⟧)", "(Undefined: Division by zero, Σ⟦⟧)"]
      resultOf execPowerSet "(if (1 + 0) then 3 else 4)" `shouldMatchList'` ["(4, Σ⟦⟧)", "(3, Σ⟦⟧)"]
      

  where
    resultOf exec' str = fmt $ exec' $ either (error . show) id (parser str)
    shouldBe' a b = shouldBe a (unlines b)
    shouldMatchList' = shouldMatchList . lines



-- | Invalid programs.
-- >>> import Stanly.Fmt(fmt)
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
