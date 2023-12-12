module Stanly.Abstract where

import Control.Applicative (Alternative)
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Stanly.Fmt
import Stanly.Interpreter
import Stanly.Unicode

type Addr = Var

newtype AbstractT m a = AbstractT (ReaderT (Env Var) (StateT (Store_ Var) m) a)
    deriving (Functor, Applicative, Monad, Alternative, MonadPlus, MonadReader (Env Var), MonadState (Store_ Var))

runAbstractT ∷ AbstractT m a → m (a, Store_ Var)
runAbstractT (AbstractT m) = runStateT (runReaderT m mempty) mempty

-- instance (MonadPlus m) ⇒ Exc (AbstractT m) where
--     exc why = 𝖕 $ Undefined ("Bottom: " <> why)

top ∷ (Applicative f) ⇒ String → f (Val l)
top why = 𝖕 $ Undefined ("Top: " <> why)

-- instance (MonadPlus m) ⇒ Primops Addr (AbstractT m) where
--     op2 o lhs rhs
--         | o `notElem` ["+", "-", "*", "/"] = exc $ "Invalid operation: " <> o
--         | otherwise = case (o, lhs, rhs) of
--             ("/", _, Undefined t) → mplus (exc "Division by zero") (reraise t)
--             ("/", _, NumV 0) → exc "Division by zero"
--             (_, NumV _, NumV _) → top "op2 on Numbers"
--             (_, Undefined t, _) → reraise t
--             (_, _, Undefined t) → reraise t
--             (_, _, _) → top "Invalid operands top op2"
--       where
--         reraise t = 𝖕 $ Undefined t

--     branch fls tru = \case
--         NumV n → if n /= 0 then tru else fls
--         Undefined _ → mplus tru fls
--         LamV{} → exc "Can't branch on function."
--         TxtV{} → exc "Can't branch on text."

-- instance (Monad m) ⇒ Store Addr (AbstractT m) where
--     alloc = 𝖕
--     deref l = do
--         store ← get
--         maybe (error $ show l ++ " not found in store. " ++ fmt store) 𝖕 (lookup l $ unStore store)
--     ext l s = modify (\(Store_ store) → Store_ ((l, s) : store))

newtype PowerSetT a = PowerSet {unPowerSet ∷ [a]} deriving (Eq, Show, Foldable, Functor, Applicative, Monad, Alternative, MonadPlus)

execPowerSet ∷ Expr → PowerSetT (Val Var, Store_ Var)
-- execPowerSet e = PowerSet $ nub $ (unPowerSet ∘ runAbstractT) (fix eval e)
execPowerSet = undefined

instance (Fmt a) ⇒ Fmt (PowerSetT a) where
    ansiFmt (PowerSet xs) = foldr ((\a b → a <> start "\n" <> b) ∘ ansiFmt) (start "") xs
