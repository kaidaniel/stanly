import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Stanly.Parser(parser)
import Stanly.Expr(Expr(..), fmt)
import Stanly.Eval(expr, var)
import Stanly.Concrete(Concrete)
import Stanly.Exec(exec)

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
