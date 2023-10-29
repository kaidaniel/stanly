module Stanly.Eval (eval) where

import Control.Monad.Reader (ask, local, asks, MonadReader(..))
import Control.Monad ( void, liftM2, join )
import Stanly.Expr (Expr (..))
import Stanly.Fmt(fmt)
import Stanly.Interpreter

eval :: (Interpreter l m) => Expr -> m (Val l)
eval expression = case expression of
  (Num n) -> return $ NumV n
  (Lam x e) -> asks (LamV x e)
  (Vbl variable) -> do
    (Env environment) <- ask
    case lookup variable environment of
      Just address -> find address
      Nothing -> exc $ show variable ++ " not found in environment. " ++ fmt expression ++ fmt (Env environment)
  (If test tru fls) -> do result <- ev test; t <- truthy result; ev (if t then tru else fls)
  (Op2 o left right) -> join $ liftM2 (op2 o) (ev left) (ev right)
  (Rec fname body) -> do
    scope <- ask
    addr <- alloc fname
    ext addr $ withBinding scope (fname, addr) $ ev body
  (App fn arg) -> do
    fn' <- ev fn
    case fn' of
      (LamV argname body r) -> do
        arg' <- ev arg
        addr <- alloc argname
        void $ ext addr $ return arg'
        withBinding r (argname, addr) $ ev body
      _ -> do
        r <- ask
        exc $ "\"" ++ fmt fn' ++ "\" is not a function. " ++ fmt expression ++ fmt r
  where
    withBinding (Env environment) binding = local (\_ -> Env (binding : environment))
