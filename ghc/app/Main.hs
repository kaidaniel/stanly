import Data.Char (isSpace)
import Stanly.Concrete (Concrete)
import Stanly.Exec (exec)
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
  putStrLn "Concrete"
  indented $ termFmt ((exec @Concrete) ast)
  where
    indented s = putStrLn $ " " ++ s