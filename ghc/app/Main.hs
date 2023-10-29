import Data.Char (isSpace)
import Stanly.Concrete (execConcrete, execTrace, execNotCovered)
import Stanly.Abstract (execPowerSet)
import Stanly.Fmt (Fmt (..))

main :: IO ()
main = do
  str <- getContents
  ast <- either (error . show) return (parser "<stdin>" str)
  putStrLn "Input"
  prnt $ (reverse . dropWhile isSpace . reverse) str
  putStrLn "Desugared"
  prnt $ termFmt ast
  putStrLn "AST"
  prnt $ show ast
  putStrLn "Concrete.Concrete"
  prnt $ termFmt (execConcrete ast)
  putStrLn "Concrete.Trace"
  prnt $ termFmt (execTrace ast)
  putStrLn "Concrete.NotCovered"
  prnt $ termFmt (execNotCovered ast)
  putStrLn "Abstract.PowerSet"
  prnt $ termFmt (execPowerSet ast)
  where
    indented' s = "  " ++ s
    indented s = unlines (map indented' (lines s))
    prnt s = putStrLn $ indented s
