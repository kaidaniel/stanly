module Stanly.Eval (eval, bottom, Interpreter (..), Env (..), Store (..)) where

import Control.Monad.Except
import Control.Monad.Reader (MonadReader (ask, local))
import Control.Monad.State (MonadState, modify, get)
import Stanly.Expr (Expr (..), Var)
import Stanly.Fmt

bottom :: (MonadError String m) => String -> m a
bottom err = throwError $ "Bottom: " ++ err
newtype Store addr val = Store [(addr, val)] deriving (Eq, Show, Foldable)
newtype Env addr = Env [(Var, addr)] deriving (Eq, Show)

class
  (Fmt val, Show addr, Eq addr, MonadState (Store addr val) m, MonadReader (Env addr) m, MonadError String m) =>
  Interpreter m val addr where
  op2 :: String -> val -> val -> m val
  truthy :: val -> m Bool
  alloc :: Var -> m addr
  ev :: Expr -> m val
  ev = eval
  destruct :: m val -> m (Expr, Maybe (Var, Env addr))
  construct :: Expr -> Maybe (m val)

eval :: (Interpreter m val addr) => Expr -> m val
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

instance (Show addr) => Fmt (Env addr) where
  ansiFmt :: (Show addr) => Env addr -> ANSI
  ansiFmt r = green >+ "⟦" <> fmt' r "" <> green >+ "⟧"
    where
      fmt' :: (Show addr) => Env addr -> String -> ANSI
      fmt' (Env ((v, a) : r')) sep = start sep <> green >+ v <> start "↦" <> green >+ show a <> fmt' (Env r') ","
      fmt' (Env []) _ = start ""

instance (Show addr, Fmt v) => Fmt (Store addr v) where
  ansiFmt s = yellow >+ "Σ⟦" <> fmt' s "" <> yellow >+ "⟧"
    where
      fmt' :: (Show addr, Fmt v) => Store addr v -> String -> ANSI
      fmt' (Store ((a, v) : r)) sep = start sep <> green >+ show a <> start "↦" <> ansiFmt v <> fmt' (Store r) ","
      fmt' (Store []) _ = start ""
