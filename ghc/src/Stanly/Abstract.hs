module Stanly.Abstract where

import Stanly.Interpreter
import Stanly.Fmt
import Stanly.Expr
import Stanly.Eval(eval)
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import Control.Applicative (Alternative)
import Data.List (nub)

-- \m::*->* a::*.{A} {R} Env Var -> {E} {S} Store_ Var -> m (Either e a, Store_ Var)
newtype AbstractT m a = AbstractT (ReaderT (Env Var) (ExceptT String (StateT (Store_ Var) m)) a)
    deriving (Functor, Applicative, Monad, Alternative, MonadPlus, MonadReader (Env Var), MonadState (Store_ Var), MonadError String)

runAbstractT :: AbstractT m a -> m (Either String a, Store_ Var)
runAbstractT (AbstractT m) = (flip runStateT (Store_ []) . runExceptT) (runReaderT m (Env []))

instance (Monad m) => Exc (AbstractT m) where
    exc err = throwError $ "Exception: " ++ err

instance (Monad m) => Primops Var (AbstractT m) where
    op2 o lhs rhs 
      | o `notElem` ["+", "-", "*", "/"] = exc $ "Invalid operation: " <> o
      | otherwise = case (o, lhs, rhs) of
        ("/", _, t@(Top _)) -> mplus (exc "Division by zero") (return t)
        ("/", _, NumV 0) -> exc "Division by zero"
        (_, lam@(LamV {}), _) -> exc ("Op2 on lambda: " <> fmt lam <> " " <> o <> " " <> fmt rhs)
        (_, _, lam@(LamV {})) -> exc ("Op2 on lambda: " <> fmt lhs <> " " <> o <> " " <> fmt lam)
        (_, _, _) -> return $ Top "op2 on numbers"

    truthy (Top _) = mplus (return True) (return False)
    truthy (NumV n) = return (n /= 0)
    truthy (LamV {}) = exc "Can't compute truthiness of lambda"


instance (Monad m) => Store Var (AbstractT m) where
    alloc = return
    find l = do
        (Store_ store) <- get
        case lookup l store of
            Just val -> return val
            Nothing -> error $ show l ++ " not found in store. " ++ fmt (Store_ store)
    ext l m = m >>= (\s -> modify (\(Store_ store) -> Store_ ((l, s) : store))) >> m

instance (Monad m) => Interpreter Var (AbstractT m) where
    ev = eval


newtype PowerSetT a = PowerSet { unPowerSet :: [a] } deriving (Eq, Show, Foldable, Functor, Applicative, Monad, Alternative, MonadPlus)

execPowerSet :: Expr -> PowerSetT (Either String (Val Var), Store_ Var)
execPowerSet e = PowerSet $ nub $ (unPowerSet . runAbstractT) (ev e)

instance (Fmt a) => Fmt (PowerSetT a) where
    ansiFmt (PowerSet xs) = foldr ((\a b -> a <> start "\n" <> b) . ansiFmt) (start "") xs
