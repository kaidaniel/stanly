{-# LANGUAGE FunctionalDependencies #-}

module Stanly.Eval (eval, bottom, Interpreter (..), Value (..), Env (..), Store (..)) where

import Control.Monad.Except
import Control.Monad.Reader (MonadReader (ask, local))
import Control.Monad.State (MonadState, modify, get)
import Stanly.Expr (Expr (..), Var)
import Stanly.Fmt

bottom :: (MonadError String m) => String -> m a
bottom err = throwError $ "Bottom: " ++ err

newtype Store addr val = Store {unStore :: [(addr, val)]} deriving (Eq, Show, Foldable)

newtype Env addr = Env [(Var, addr)] deriving (Eq, Show)

class Value val addr | val -> addr where
  var :: val -> Var
  expr :: val -> Expr
  env :: val -> Env addr

class
  (Show addr, Eq addr, MonadState (Store addr val) m, MonadReader (Env addr) m, MonadError String m, Value val addr) =>
  Interpreter m val addr
    | val -> m,
      addr -> m
  where
  op2 :: String -> val -> val -> m val
  lambda :: Var -> Expr -> m val
  number :: Int -> m val
  truthy :: val -> m Bool
  alloc :: Var -> m addr
  ev :: Expr -> m val
  ev = eval
  run :: m val -> (Either String val, Store addr val)

eval :: (Interpreter m val addr) => Expr -> m val
eval expression = case expression of
  (Num n) -> number n
  (Vbl variable) -> do 
    (Env environment) <- ask
    (Store store) <- get
    case lookup variable environment of 
      Just address -> case lookup address store of
        Just val -> return val
        Nothing -> error $ show variable ++ " not found in store: " ++ fmt expression ++ fmt (Env environment)
      Nothing -> bottom $ show variable ++ " not found in environment: " ++ fmt expression ++ fmt (Env environment)
  (If test tru fls) -> do result <- ev test; t <- truthy result; ev (if t then tru else fls)
  (Op2 o left right) -> do left' <- ev left; right' <- ev right; op2 o left' right'
  (Rec fname body) -> do
    scope <- ask
    addr <- alloc fname
    v <- local (\_ -> ext scope (fname, addr)) (ev body)
    memkpy (addr, v)
    return v
  (Lam x e) -> lambda x e
  (App (Num _) _) -> do r <- ask; bottom $ "Applying a number: " ++ fmt expression ++ fmt r
  (App fn arg) -> do
    fn' <- ev fn
    let argname = var fn'
    arg' <- ev arg
    addr <- alloc argname
    memkpy (addr, arg')
    local (\_ -> ext (env fn') (argname, addr)) (ev (expr fn'))
  where
    memkpy binding = modify (\(Store store) -> Store (binding : store))
    ext (Env environment) binding = Env (binding : environment)
    

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
