import Stanly.Parser(parse, expr)
import Stanly.Concrete(Concrete)
import Stanly.Exec(exec)


main :: IO ()
main = do
    str <- getContents
    ast <- case parse expr "<stdin>" str of Left err -> error $ show err; Right ast -> return ast
    let (value, store) = (exec @Concrete) ast
    putStrLn $ "Result: " ++ show value
    putStrLn $ "Store:  " ++ show store
