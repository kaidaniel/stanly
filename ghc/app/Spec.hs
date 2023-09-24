import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Stanly.Parser
import Stanly.Expr(Expr(..), fmt, Value(..))
import Stanly.ConcreteSemantics

instance Arbitrary Expr where
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
                If  <$> rec' <*> rec' <*> rec'
              ]

shrink :: Expr -> [Expr]
shrink = genericShrink

closureOf eval str = 
  let (LamV x v _, _) = eval (case parser str of Left err -> error $ show err; Right ast -> ast) 
  in  fmt (Lam x v)

main :: IO ()
main = hspec $ do
    describe "parser" $
      prop "is inverted by fmt" $
        \(e :: Expr) -> (fmt <$> ((parser . fmt) e)) === Right (fmt e)
    describe "ConcreteSemantics.eval" $
      it "results in closure over free variables" $ do
        closureOf eval "let x = (1 + 10) in ((λy.(λf.(f x))) (2 + 20))" `shouldBe` "(λf.(f x))"
        closureOf eval "((λg.(λx.(g x))) 3)" `shouldBe` "(λx.(g x))"