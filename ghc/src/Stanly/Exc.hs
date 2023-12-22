module Stanly.Exc (MonadExc, ExcT, ExcRes, runExcT, exc) where

import Control.Monad.Except (ExceptT, MonadError, runExceptT, throwError)
import Stanly.Fmt (Fmt, FmtStr, (⊹))
import Stanly.Unicode

type MonadExc m = MonadError FmtStr m
type ExcT m = ExceptT FmtStr m
type ExcRes a = Either FmtStr a

exc ∷ (Fmt msg, MonadExc m) ⇒ msg → m a
exc msg = throwError ⎴ "Exception: " ⊹ msg

runExcT ∷ ExcT m a → m (ExcRes a)
runExcT = runExceptT
