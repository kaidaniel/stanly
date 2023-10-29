import qualified Stanly.Abstract as A
import qualified Stanly.Expr as E
import qualified GHC.Base as B

main = do
    print $ B.mplus "a" "b"
    print $ B.mplus ["a", "b"] ["c", "d"]
    print $ A.execPowerSet (E.Op2 "+" (E.Num 1) (E.Op2"/" (E.Num 2) (E.Num 3)))
