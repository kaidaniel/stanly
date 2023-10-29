module Stanly.Eval (eval) where

import Control.Monad.Reader (ask, local, asks, MonadReader(..))
import Control.Monad ( liftM2, join, (>=>) )
import Stanly.Expr (Expr (..))
import Stanly.Fmt(fmt)
import Stanly.Interpreter
import Data.Bool (bool)

eval :: (Interpreter l m) => Expr -> m (Val l)
eval expression = case expression of
  (Num n)            -> return $ NumV n
  (Lam x e)          -> asks (LamV x e)
  (Vbl vbl)          -> ask >>= \(Env mr) -> maybe (varNotFound vbl mr) find (lookup vbl mr)
  (If test tru fls)  -> ev test >>= (truthy >=> (ev . bool fls tru))
  (Op2 o left right) -> join $ liftM2 (op2 o) (ev left) (ev right)
  (Rec fname body)   -> ask >>= \(Env mr) -> alloc fname >>= \ml -> ext ml (local' body mr fname ml)
  (App function arg) -> ev function >>= \mf -> case mf of
      (LamV f body (Env r)) -> ev arg >>= \mx -> alloc f >>= \ml -> ext ml (pure mx) >> local' body r f ml
      _                     -> ask >>= notAFunction mf
  where
    local' body r vbl addr = (local . const . Env) ((vbl, addr) : r) (ev body)
    varNotFound vbl r = exc $ show vbl ++ " not found in environment. " ++ fmt expression ++ fmt (Env r)
    notAFunction e r = exc $ "\"" ++ fmt e ++ "\" is not a function. " ++ fmt expression ++ fmt r
