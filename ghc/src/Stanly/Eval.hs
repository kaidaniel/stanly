{-# LANGUAGE FunctionalDependencies #-}

module Stanly.Eval (eval, Interpreter(..), Value(..), Env(..), Store(..)) where
import Stanly.Expr (Expr (..), Var)
import Control.Monad.Reader (MonadReader (ask, local))
import Control.Monad.State (MonadState, gets, modify)
import Control.Monad.Except

-- | The scope: Addresses for free variables in the expression under evaluation.
-- type MonadVblReader addr m = MonadReader (Env addr) m
-- | The heap: Values for adresses.
-- type MonadStore

newtype Store addr val = Store { unStore :: [(addr, val)] } deriving (Eq, Show, Foldable)
newtype Env addr = Env { unEnv :: [(Var, addr)] } deriving (Eq, Show)

class Value val addr | val -> addr where
  var  :: val -> Var
  expr :: val -> Expr
  env  :: val -> Env addr

class (Eq addr, MonadState (Store addr val) m, MonadReader (Env addr) m, Value val addr) 
  => Interpreter m val addr | val -> m, addr -> m where
  op2    :: String -> val -> val -> m val
  lambda :: Var -> Expr -> m val
  number :: Int -> m val
  truthy :: val -> m Bool
  alloc  :: Var -> m addr
  ev     :: Expr -> m val
  ev = eval
  run :: m val -> (Either String val, Store addr val)

eval :: Interpreter m val addr => Expr -> m val
eval expression = case expression of
  (Num n) -> number n
  (Vbl variable) -> do { scope <- ask; find scope variable }
  (If test tru fls) -> do { result <- ev test; t <- truthy result; ev (if t then tru else fls) }
  (Op2 o left right) -> do { left' <- ev left; right' <- ev right; op2 o left' right' }
  (Rec fname body) -> do
    scope <- ask
    addr <- alloc fname
    v <- local (\_ -> ext scope (fname, addr)) (ev body)
    memkpy (addr, v)
    return v
  (Lam x e) -> lambda x e
  (App (Num _) _) -> invalidSyntax
  (App fn arg) -> do
    fn' <- ev fn
    let argname = var fn'
    arg' <- ev arg
    addr <- alloc argname
    memkpy (addr, arg')
    local (\_ -> ext (env fn') (argname, addr)) (ev (expr fn'))
  where
    memkpy binding = modify (\(Store store) -> Store (binding : store))
    ext (Env environment) binding  = Env (binding : environment)
    jst x = case x of Just x' -> x'; Nothing -> error "intended to be impossible to happen at run time"
    find (Env r) x = gets (jst . lookup (jst $ lookup x r) . unStore)
    invalidSyntax = error $ "Invalid syntax: " ++ show expression
