module Stanly.Eval (eval) where

import Control.Monad ( liftM2, join, (>=>) )
import Stanly.Expr (Expr (..))
import Stanly.Fmt(fmt)
import Stanly.Interpreter
import Data.Bool (bool)

eval :: (Interpreter l m) => Expr -> m (Val l)
eval expression = case expression of
  (Num n)            -> pure (NumV n)
  (Lam x e)          -> fmap (LamV x e) env
  (Vbl vbl)          -> search exc vbl >>= deref
  (If test tru fls)  -> ev test >>= (truthy >=> (ev . bool fls tru))
  (Op2 o left right) -> join $ liftM2 (op2 o) (ev left) (ev right)
  (Rec f e)          -> env >>= \mr    -> alloc f   >>= \ml -> ext ml (assign (f, ml) mr (ev e))
  (App lamV x)       -> ev lamV >>= \mlamV -> case mlamV of
      (LamV mx' me mr) -> ev x >>= \mx -> alloc mx' >>= \ml -> ext ml (pure mx) >> assign (mx', ml) mr (ev me)
      _                -> env >>= exc . notAFunction mlamV
  where
    notAFunction lamV r = "\"" <> fmt lamV <> "\" is not a function. " <> fmt expression <> fmt r
