import Data.Char (isSpace)
import Stanly.Concrete (execConcrete, execTrace, execNotCovered)
import Stanly.Expr (expr, parse)
import Stanly.Fmt (Fmt (..))

main :: IO ()
main = do
  str <- getContents
  ast <- either (error . show) return (parse expr "<stdin>" str)
  putStrLn "Input"
  prnt $ (reverse . dropWhile isSpace . reverse) str
  putStrLn "Desugared"
  prnt $ termFmt ast
  putStrLn "AST"
  prnt $ show ast
  putStrLn "Concrete"
  prnt $ termFmt (execConcrete ast)
  putStrLn "Trace"
  prnt $ termFmt (execTrace ast)
  putStrLn "NotCovered"
  prnt $ termFmt (execNotCovered ast)
  where
    indented' s = "  " ++ s
    indented s = unlines (map indented' (lines s))
    prnt s = putStrLn $ indented s
