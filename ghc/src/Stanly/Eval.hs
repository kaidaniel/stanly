{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Eta reduce" #-}

module Stanly.Eval (eval, Interpreter(..), Value(..), Env(..), Store(..)) where
import Stanly.Expr (Expr (..), Var, Fmt(..))
import Control.Monad.Reader (MonadReader (ask, local))
import Control.Monad.State (MonadState, gets, modify)

type Addr = Int

jst :: Maybe a -> a
jst x = case x of Just x' -> x'; Nothing -> error "intended to be impossible to happen at run time"

ext :: Var -> Addr -> Env -> Env
ext x addr (Env r) = Env ((x, addr) : r)

newtype Store v = Store { unStore :: [(Addr, v)] } 
  deriving (Eq, Show, Foldable)
newtype Env = Env { unEnv :: [(Var, Addr)] } 
  deriving (Eq, Show)

find :: (MonadState (Store v) m, Value v) => Env -> Var -> m v
find (Env r) x = gets (jst . lookup (jst $ lookup x r) . unStore)

initz :: MonadState (Store v) m => Addr -> v -> m ()
initz a v = modify (\(Store s) -> Store ((a, v) : s))

class Value v where
  var :: v -> Var
  expr :: v -> Expr
  env :: v -> Env
  truthy :: v -> Bool

class (MonadState (Store v) m, MonadReader Env m, Value v) => Interpreter m v where
  op2 :: String -> v -> v -> m v
  lambda :: Var -> Expr -> m v
  number :: Int -> m v
  alloc :: Var -> v -> m Int

eval :: Interpreter m v => (Expr -> m v) -> Expr -> m v
eval _ (Num n) = number n
eval _ (Vbl x) = do { r <- ask; find r x }
eval ev' (If etest etrue efalse) = do { n <- ev' etest; ev' $ if truthy n then etrue else efalse }
eval ev' (Op2 o left right) = do { left' <- ev' left; right' <- ev' right; op2 o left' right' }
eval ev' (Rec f body) = do
  r <- ask
  v <- ev' body
  a <- alloc f v
  v' <- local (\_ -> ext f a r) (pure v)
  initz a v'
  return v'
eval _ (Lam x e) = lambda x e
eval ev' (App fn arg) = do
  fn' <- ev' fn
  let x = var fn'
  v <- ev' arg
  a <- alloc x v
  initz a v
  local (\_ -> ext x a (env fn')) (ev' (expr fn'))

instance Fmt Env where
  fmt :: Env -> String
  fmt env' = "⟦" ++ fmt' env' "" ++ "⟧"
    where
      fmt' (Env ((x, a) : r)) sep = sep ++ x ++ "→" ++ show a ++ fmt' (Env r) " "
      fmt' (Env []) _ = ""  
      