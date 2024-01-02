module Stanly.Exc (
    MonadExc,
    ExcT,
    ExcRes,
    runExcT,
    divisionByZero,
    invalidArgsToOperator,
    branchOnNonNumeric,
    notAFunction,
    varNotFoundInEnvironment,
    varNotFoundInStore,
) where

import Control.Monad.Except (ExceptT, MonadError, runExceptT, throwError)
import Stanly.Fmt (Fmt (..))
import Stanly.Unicode

data Exception where
    DivisionByZero ∷ Exception
    InvalidArgsToOperator ∷ Exception
    BranchOnNonNumeric ∷ Exception
    NotAFunction ∷ Exception
    VarNotFoundInEnvironment ∷ Exception
    VarNotFoundInStore ∷ Exception
    deriving (Eq, Ord, Show, Enum)

instance Fmt Exception where
    fmt = fmt ∘ ("Exception: " <>) ∘ show

type MonadExc m = MonadError Exception m
type ExcT m = ExceptT Exception m
type ExcRes a = Either Exception a

exc ∷ (MonadExc m) ⇒ Exception → m a
exc = throwError

divisionByZero
    , invalidArgsToOperator
    , branchOnNonNumeric
    , notAFunction
    , varNotFoundInEnvironment
    , varNotFoundInStore ∷
        (MonadExc m) ⇒ m a
divisionByZero = exc DivisionByZero
invalidArgsToOperator = exc InvalidArgsToOperator
branchOnNonNumeric = exc BranchOnNonNumeric
notAFunction = exc NotAFunction
varNotFoundInEnvironment = exc VarNotFoundInEnvironment
varNotFoundInStore = exc VarNotFoundInStore

runExcT ∷ (Monad m) ⇒ ExcT m a → m (ExcRes a)
runExcT = runExceptT
