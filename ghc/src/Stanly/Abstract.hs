module Stanly.Abstract where

import Stanly.Eval

-- import Control.Monad.Trans.Maybe(MaybeT)
import Control.Monad (ap)
import Control.Monad.Except (MonadError (..))

-- import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.Trans (MonadTrans (..))

-- import Stanly.Eval
import Stanly.Unicode

-- newtype AbsRes a =

newtype AbstractT m a = AbstractT (Env → Store → m [Res a])
    deriving (Functor)

-- instance (Monad m) ⇒ Applicative (AbstractT m) where
--     pure x = AbstractT \_ s → pure ⎴ [Step x s]
--     (<*>) = ap
-- instance (Monad m) ⇒ Monad (AbstractT m) where
--     (AbstractT m₁) >>= f = AbstractT \e s₁ → do
--         res ← m₁ e s₁
--         undefined
-- instance (Monad m) ⇒ MonadError Exception (AbstractT m) where
--     throwError exc = AbstractT \_ _ → ω (Stop exc)
--     catchError (AbstractT m₁) f = AbstractT \e s₁ → do
--         res ← m₁ e s₁
--         case res of
--             Step a s₂ → ω (Step a s₂)
--             Stop exc → let AbstractT m₂ = f exc in m₂ e s₁
-- instance MonadTrans AbstractT where
--     lift m = AbstractT \_ s → do
--         v ← m
--         ω ⎴ [Step v s]
