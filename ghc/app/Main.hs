import Data.Char (isSpace)
import Stanly.Concrete (execConcrete, execTrace)
import Stanly.Expr (expr, parse)
import Stanly.Fmt (Fmt (..))

main :: IO ()
main = do
  str <- getContents
  ast <- either (error . show) return (parse expr "<stdin>" str)
  putStrLn "Input"
  indented $ (reverse . dropWhile isSpace . reverse) str
  putStrLn "Desugared"
  indented $ termFmt ast
  putStrLn "AST"
  indented $ show ast
  putStrLn "Concrete"
  indented $ termFmt (execConcrete ast)
  putStrLn "Trace"
  indented $ show (execTrace ast)
  where
    indented s = putStrLn $ " " ++ s