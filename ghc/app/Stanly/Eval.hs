{-# LANGUAGE FunctionalDependencies #-}

module Stanly.Eval (eval, Interpreter(..)) where
import Stanly.Expr (Expr (..), Env, Var, Addr)
import Control.Monad.Reader

class (MonadFail m) => Interpreter m v | m -> v where
  truthy :: v -> m Bool
  alloc :: Var -> m Int
  evOp2 :: String -> v -> v -> m v
  find :: Env -> Var -> m v
  ext :: Addr -> v -> m ()
  bindVar :: Var -> Addr -> Env -> m Env
  env :: m Env
  inEnv :: m Env -> m v -> m v
  returnClosure :: Var -> Expr -> m v
  literalValue :: Expr -> m v
  getClosure :: v -> m(Var, Expr, Env)

eval :: Interpreter m v => (Expr -> m v) -> Expr -> m v
eval ev' e@(Num _) = literalValue e
eval ev' (Vbl x) = do
  r <- env
  find r x
eval ev' (If etest etrue efalse) = do
  n <- ev' etest
  z <- truthy n
  ev' $ if z then etrue else efalse
eval ev' (Op2 op2 left right) = do
  left' <- ev' left
  right' <- ev' right
  evOp2 op2 left' right'
eval ev' (Rec f body) = do
  r <- env
  a <- alloc f
  v <- inEnv (bindVar f a r) (ev' body)
  ext a v
  return v
eval ev' e@(Lam _ _) = literalValue e
eval ev' (App fn arg) = do
  fn' <- ev' fn
  (x, body, r) <- getClosure fn'
  v <- ev' arg
  a <- alloc x
  ext a v
  inEnv (bindVar x a r) (ev' body)
