module Stanly.Eval (eval) where

import Control.Monad.Reader (ask, local)
import Control.Monad.State (modify, get)
import Stanly.Expr (Expr (..))
import Stanly.Fmt(fmt)
import Stanly.Interpreter(bottom, MonadInterpreter(..), Env(..), Store(..))

eval :: (MonadInterpreter addr val m) => Expr -> m val
eval expression = case expression of
  (Num _) -> construct' expression
  (Lam _ _) -> construct' expression
  (Vbl variable) -> do 
    (Env environment) <- ask
    (Store store) <- get
    case lookup variable environment of 
      Just address -> case lookup address store of
        Just val -> return val
        Nothing -> error $ show variable ++ " not found in store. " ++ fmt expression ++ fmt (Env environment) ++ ", " ++ fmt (Store store)
      Nothing -> bottom $ show variable ++ " not found in environment. " ++ fmt expression ++ fmt (Env environment)
  (If test tru fls) -> do result <- ev test; t <- truthy result; ev (if t then tru else fls)
  (Op2 o left right) -> do left' <- ev left; right' <- ev right; op2 o left' right'
  (Rec fname body) -> do
    scope <- ask
    addr <- alloc fname
    v <- local (\_ -> ext scope (fname, addr)) (ev body)
    memkpy (addr, v)
    return v
  (App fn arg) -> do
    fn' <- destruct $ ev fn
    case fn' of
      (expr, Just (argname, r)) -> do 
        arg' <- ev arg
        addr <- alloc argname; memkpy (addr, arg')
        local (\_ -> ext r (argname, addr)) (ev expr)
      (expr, Nothing) -> do
        r <- ask
        bottom $ "\"" ++ fmt expr ++ "\" is not a function. " ++ fmt expression ++ fmt r
  where
    memkpy binding = modify (\(Store store) -> Store (binding : store))
    ext (Env environment) binding = Env (binding : environment)
    construct' e = case construct e of Just v -> v; Nothing -> error $ "Expression " ++ fmt e ++ " is not a value."
