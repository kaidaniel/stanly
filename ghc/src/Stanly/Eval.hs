{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FunctionalDependencies #-}

module Stanly.Eval (eval, Interpreter(..), Value(..), Env(..), Store(..)) where
import Stanly.Expr (Expr (..), Var)
import Control.Monad.Reader (MonadReader (ask, local))
import Control.Monad.State (MonadState, gets, modify)

jst :: Maybe a -> a
jst x = case x of Just x' -> x'; Nothing -> error "intended to be impossible to happen at run time"

ext :: Env addr -> (Var, addr) -> Env addr
ext (Env environment) binding  = Env (binding : environment)

newtype Store addr v = Store { unStore :: [(addr, v)] } 
  deriving (Eq, Show, Foldable)

newtype Env addr = Env { unEnv :: [(Var, addr)] } 
  deriving (Eq, Show)

find :: forall a1 a2 (m :: * -> *).(MonadState (Store a1 a2) m, Eq a1) => Env a1 -> Var -> m a2
find (Env r) x = gets (jst . lookup (jst $ lookup x r) . unStore)

memkpy :: MonadState (Store addr v) m => (addr, v) -> m ()
memkpy binding = modify (\(Store s) -> Store (binding : s))

class Value v addr | v -> addr where
  var :: v -> Var
  expr :: v -> Expr
  env :: v -> Env addr

class (Eq addr, MonadState (Store addr val) m, MonadReader (Env addr) m, Value val addr) 
  => Interpreter m val addr | val -> m, addr -> m where
  op2 :: String -> val -> val -> m val
  lambda :: Var -> Expr -> m val
  number :: Int -> m val
  truthy :: val -> m Bool
  alloc :: Var -> m addr
  ev :: Expr -> m val
  ev = eval
  run :: m val -> (val, Store addr val)

eval :: Interpreter m val addr => Expr -> m val
eval (Num n) = number n
eval (Vbl x) = do { r <- ask; find r x }
eval (If etest etrue efalse) = do { n <- ev etest; t <- truthy n; ev $ if t then etrue else efalse }
eval (Op2 o left right) = do { left' <- ev left; right' <- ev right; op2 o left' right' }
eval (Rec fname body) = do
  r <- ask
  addr <- alloc fname
  v <- local (\_ -> ext r (fname, addr)) (ev body)
  memkpy (addr, v)
  return v
eval (Lam x e) = lambda x e
eval (App fn arg) = do
  fn' <- ev fn
  let argname = var fn'
  arg' <- ev arg
  addr <- alloc argname
  memkpy (addr, arg')
  local (\_ -> ext (env fn') (argname, addr)) (ev (expr fn'))
