module Stanly.Abstract where

import Control.Applicative (Alternative)
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Stanly.Env
import Stanly.Fmt
import Stanly.Language
import Stanly.Store
import Stanly.Unicode
import Stanly.Val

type Addr = Variable

newtype AbstractT m a = AbstractT (ReaderT (Env Variable) (StateT (Store Variable) m) a)
    deriving (Functor, Applicative, Monad, Alternative, MonadPlus, MonadReader (Env Variable), MonadState (Store Variable))

runAbstractT ∷ AbstractT m a → m (a, Store Variable)
runAbstractT (AbstractT m) = runStateT (runReaderT m ε₁) ε₁

-- instance (MonadPlus m) ⇒ Exc (AbstractT m) where
--     exc why = ω ⎴ Undefined ("Bottom: " ⋄ why)

-- instance (MonadPlus m) ⇒ Primops Addr (AbstractT m) where
--     op2 o lhs rhs
--         | o `notElem` ["+", "-", "*", "/"] = exc ⎴ "Invalid operation: " ⋄ o
--         | otherwise = case (o, lhs, rhs) of
--             ("/", _, Undefined t) → mplus (exc "Division by zero") (reraise t)
--             ("/", _, NumV 0) → exc "Division by zero"
--             (_, NumV _, NumV _) → top "op2 on Numbers"
--             (_, Undefined t, _) → reraise t
--             (_, _, Undefined t) → reraise t
--             (_, _, _) → top "Invalid operands top op2"
--       where
--         reraise t = ω ⎴ Undefined t

--     branch fls tru = \case
--         NumV n → if n /= 0 then tru else fls
--         Undefined _ → mplus tru fls
--         LamV{} → exc "Can't branch on function."
--         TxtV{} → exc "Cant branch on text."

-- instance (Monad m) ⇒ Store Addr (AbstractT m) where
--     alloc = ω
--     deref l = do
--         store ← get
--         maybe (error ⎴ show l ++ " not found in store. " ++ bwText store) ω (lookup l ⎴ unStore store)
--     ext l s = modify (\(Store store) → Store ((l, s) : store))

newtype PowerSetT a = PowerSet {unPowerSet ∷ [a]} deriving (Eq, Show, Foldable, Functor, Applicative, Monad, Alternative, MonadPlus)

execPowerSet ∷ Expr → PowerSetT (Val Variable, Store Variable)
-- execPowerSet e = PowerSet ⎴ nub ⎴ (unPowerSet ∘ runAbstractT) (fix eval e)
execPowerSet = undefined

instance (Fmt a) ⇒ Fmt (PowerSetT a) where
    fmt (PowerSet xs) = κ₁ [x ⊹ "\n" | x ← xs]
