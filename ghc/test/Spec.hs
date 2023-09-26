import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Stanly.Parser
import Stanly.Expr(Expr(..), fmt)
import Stanly.Eval(Value(..))
import Stanly.Sema(eval)



closureOf eval' str =
  let (v, _) = eval' (case parser str of Left err -> error $ show err; Right ast -> ast)
  in  fmt (Lam (var v) (Stanly.Eval.expr v))

main :: IO ()
main = hspec $ do
    describe "parser" $
      prop "is inverted by fmt" $
        \(e :: Expr) -> (fmt <$> (parser . fmt) e) === Right (fmt e)
    describe "ConcreteSemantics.eval" $
      it "results in closure over free variables" $ do
        closureOf eval "let x = (1 + 10) in ((λy.(λf.(f x))) (2 + 20))" `shouldBe` "(λf.(f x))"
        closureOf eval "((λg.(λx.(g x))) 3)" `shouldBe` "(λx.(g x))"
