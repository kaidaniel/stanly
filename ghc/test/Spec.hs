{-# LANGUAGE DeriveGeneric #-}
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Stanly.Expr(Expr(..), parser)
import Stanly.Eval(expr, var)
import Stanly.Concrete(Concrete)
import Stanly.Exec(exec)
import Stanly.Fmt(fmt)
import GHC.Generics (Generic)


newtype TestExpr = TestExpr { unTestExpr :: Expr }
  deriving (Generic, Eq, Show)

instance Arbitrary TestExpr where
  arbitrary = sized (fmap TestExpr . arbitrary')
    where
      word :: Gen String
      word = listOf1 (elements ['a' .. 'z'])
      arbitrary' 0 = pure $ Num 1
      arbitrary' n =
        let rec' = resize (n `div` 2) (fmap unTestExpr arbitrary)
         in oneof
              [
                Num <$> choose (0, 1000),
                Vbl <$> word,
                Op2 <$> elements ["+", "-", "*", "/"] <*> rec' <*> rec',
                App <$> rec' <*> rec',
                Lam <$> word <*> rec',
                Rec <$> word <*> rec',
                If  <$> rec' <*> rec' <*> rec'
              ]
  
instance Arbitrary Expr where
  arbitrary = unTestExpr <$> arbitrary


shrink :: TestExpr -> [TestExpr]
shrink = genericShrink


main :: IO ()
main = hspec $ do
    describe "parser" $
      prop "is inverted by fmt" $
        \(e :: Expr) -> (fmt <$> (parser . fmt) e) === Right (fmt e)
    describe "ConcreteSemantics.eval" $
      it "results in closure over free variables" $ do
        closureOf (exec @Concrete) "let x = (1 + 10) in ((λy.(λf.(f x))) (2 + 20))" `shouldBe` "(λf.(f x))"
        closureOf (exec @Concrete) "((λg.(λx.(g x))) 3)" `shouldBe` "(λx.(g x))"
  where
    closureOf eval' str =
      let (v, _) = eval' (case parser str of Left err -> error $ show err; Right ast -> ast)
      in  fmt (Lam (var v) (expr v))



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