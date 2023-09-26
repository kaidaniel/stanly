import Stanly.Parser
import Stanly.Sema
import Stanly.Expr
import System.IO

data Options = Options
    { optParse :: Bool
    , optFile :: Maybe FilePath
    }

f = parse expr "<stdin>"

main :: IO ()
main = do
    str <- getContents
    ast <- case parse expr "<stdin>" str of Left err -> error $ show err; Right ast -> return ast
    let (value, store) = eval ast
    putStrLn $ "Result: " ++ show value
    putStrLn $ "Store:  " ++ show store
