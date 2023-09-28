{-# LANGUAGE TypeFamilies #-}

module Stanly.Eval (eval, Interpreter(..), Value(..), Env(..), Store(..)) where
import Stanly.Expr (Expr (..), Var)
import Control.Monad.Reader (MonadReader (ask, local))
import Control.Monad.State (MonadState, gets, modify)

type Addr = Int

jst :: Maybe a -> a
jst x = case x of Just x' -> x'; Nothing -> error "intended to be impossible to happen at run time"

ext :: Env -> (Var, Addr) -> Env
ext (Env environment) binding  = Env (binding : environment)

newtype Store v = Store { unStore :: [(Addr, v)] } 
  deriving (Eq, Show, Foldable)

newtype Env = Env { unEnv :: [(Var, Addr)] } 
  deriving (Eq, Show)

find :: (MonadState (Store v) m, Value v) => Env -> Var -> m v
find (Env r) x = gets (jst . lookup (jst $ lookup x r) . unStore)

memkpy :: MonadState (Store v) m => (Addr, v) -> m ()
memkpy binding = modify (\(Store s) -> Store (binding : s))

class Value v where
  var :: v -> Var
  expr :: v -> Expr
  env :: v -> Env
  truthy :: v -> Bool


class (MonadState (Store v) m, MonadReader Env m, Value v) => Interpreter m v where
  op2 :: String -> v -> v -> m v
  lambda :: Var -> Expr -> m v
  number :: Int -> m v
  alloc :: Var -> m Addr
  ev :: Expr -> m v
  ev = eval
  run :: m v -> (v, Store v)

eval :: Interpreter m v => Expr -> m v
eval (Num n) = number n
eval (Vbl x) = do { r <- ask; find r x }
eval (If etest etrue efalse) = do { n <- ev etest; ev $ if truthy n then etrue else efalse }
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

