import Stanly.Expr(parse, expr)
import Stanly.Concrete(Concrete)
import Stanly.Exec(exec)
import Stanly.Fmt(fmt)
import Data.Char (isSpace)
import Text.Pretty.Simple (pPrint)


main :: IO ()
main = do
    str <- getContents
    ast <- case parse expr "<stdin>" str of Left err -> error $ show err; Right ast -> return ast
    putStrLn "Input"
    indented $ (reverse . dropWhile isSpace . reverse)  str
    putStrLn "Desugared"
    indented $ fmt ast
    putStrLn "AST:"
    pPrint ast
    putStrLn "Concrete:"
    let (value, store) = (exec @Concrete) ast
    indented $ fmt value ++ " " ++ fmt store
    where
        indented s = putStrLn $ " " ++ s