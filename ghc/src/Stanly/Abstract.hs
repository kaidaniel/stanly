{-# LANGUAGE LambdaCase #-}
module Stanly.Abstract where

import Stanly.Interpreter
import Stanly.Fmt
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Identity
import Control.Applicative (Alternative)
import Data.List (nub)

-- \m::*->* a::*.{A} {R} Env Var -> {E} {S} Store_ Var -> m (Either e a, Store_ Var)
newtype AbstractT m a = AbstractT (ReaderT (Env Var) (StateT (Store_ Var) m) a)
    deriving (Functor, Applicative, Monad, Alternative, MonadPlus, MonadReader (Env Var), MonadState (Store_ Var), Environment (Env Var) Var)

runAbstractT :: AbstractT m a -> m (a, Store_ Var)
runAbstractT (AbstractT m) = runStateT (runReaderT m (Env [])) (Store_ [])

instance (MonadPlus m) => Exc (AbstractT m) where
    exc why = pure $ Undefined ("Bottom: " <> why)

top :: Applicative f => String -> f (Val l)
top why = pure $ Undefined ("Top: " <> why)

instance (MonadPlus m) => Primops Var (AbstractT m) where
    op2 o lhs rhs
      | o `notElem` ["+", "-", "*", "/"] = exc $ "Invalid operation: " <> o
      | otherwise = case (o, lhs, rhs) of
        ("/", _, Undefined t) -> mplus (exc "Division by zero") (reraise t)
        ("/", _, NumV 0) -> exc "Division by zero"
        (_, NumV _, NumV _) -> top "op2 on Numbers"
        (_, Undefined t, _) -> reraise t
        (_, _, Undefined t) -> reraise t
        (_, _, _) -> top "Invalid operands top op2"
        where
        reraise t = pure $ Undefined t

    branch fls tru = \case
        NumV n -> if n /= 0 then tru else fls
        Undefined _ -> mplus tru fls
        LamV {} -> exc "Can't branch on function."

instance (Monad m) => Store Var (AbstractT m) where
    alloc = pure
    deref l = do
        (Store_ store) <- get
        maybe (error $ show l ++ " not found in store. " ++ fmt (Store_ store)) pure (lookup l store)
    ext l m = m >>= (\s -> modify (\(Store_ store) -> Store_ ((l, s) : store))) >> m

instance (MonadPlus m) => Interpreter Var (AbstractT m) where
    ev = eval


newtype PowerSetT a = PowerSet { unPowerSet :: [a] } deriving (Eq, Show, Foldable, Functor, Applicative, Monad, Alternative, MonadPlus)

execPowerSet :: Expr -> PowerSetT (Val Var, Store_ Var)
execPowerSet e = PowerSet $ nub $ (unPowerSet . runAbstractT) (ev e)

instance (Fmt a) => Fmt (PowerSetT a) where
    ansiFmt (PowerSet xs) = foldr ((\a b -> a <> start "\n" <> b) . ansiFmt) (start "") xs
