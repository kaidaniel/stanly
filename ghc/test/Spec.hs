{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

import GHC.Generics (Generic)
import Stanly.Concrete (Concrete)
import Stanly.Exec (exec)
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
    it "parses examples" $ do
      fmt <$> parser "(if(λs.1)then(ifxthenyelsez)else(2))" `shouldBe` Right "(if (\955s.1) then ifxthenyelsez else 2)"
      fmt <$> parser "((f)    (a))" `shouldBe` Right "(f a)"
      fmt <$> parser "(if ifx then theny else (if thenx then elsey else ifz))" `shouldBe` Right "(if ifx then theny else (if thenx then elsey else ifz))"
      fmt <$> parser "let x = (μ f. (f x)) in z" `shouldBe` Right "((\955x.z) (\956f.(f x)))"
      fmt <$> parser "(mu f.((f f) 3))" `shouldBe` Right "(\956f.((f f) 3))"
      fmt <$> parser "(fn x.x)" `shouldBe` Right "(\955x.x)"

    it "is inverted by fmt" $
      property $
        \(e :: Expr) -> (fmt <$> (parser . fmt) e) === Right (fmt e)

  describe "Concrete.eval" $ do
    it "is correct for some examples" $ do
      resultOf (exec @Concrete) "let x = 1 in ((fn x.(x + x)) 2)" `shouldBe` "(4, Σ⟦1↦2,0↦1⟧)"
      resultOf (exec @Concrete) "let x = (1 + 10) in ((λy.(λf.(f x))) (2 + 20))" `shouldBe` "(λf.(f x)⟦y↦1,x↦0⟧, Σ⟦1↦22,0↦11⟧)"
      resultOf (exec @Concrete) "((fn g.(fn x.(g x))) 3)" `shouldBe` "(λx.(g x)⟦g↦0⟧, Σ⟦0↦3⟧)"
      resultOf (exec @Concrete) "let f = (fn g.(g 0)) in ((fn y.(fn g.y)) f)" `shouldBe` "(λg.y⟦y↦1,f↦0⟧, Σ⟦1↦λg.(g 0)⟦⟧,0↦λg.(g 0)⟦⟧⟧)"
  where
    resultOf exec' str = fmt $ exec' (case parser str of Left err -> error $ show err; Right ast -> ast)

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
