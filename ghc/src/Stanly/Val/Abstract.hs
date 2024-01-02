module Stanly.Val.Abstract (
    lambda,
    op2,
    if',
    ValA,
) where

import Control.Applicative (Alternative (..))
import Stanly.Env (Env)
import Stanly.Exc (
    MonadExc,
    branchOnNonNumeric,
    divisionByZero,
    invalidArgsToOperator,
    notAFunction,
 )
import Stanly.Fmt (Fmt (..))
import Stanly.Language (Expr, Op2 (..), Variable)
import Stanly.Unicode
import Stanly.Val.Internal (Val (..), Value (..))

data ValA l where
    TopNum ∷ ValA l
    TopTxt ∷ ValA l
    Flat ∷ Val l → ValA l

instance Value ValA where
    fromVal v = Flat v

deriving instance (Eq l) ⇒ Eq (ValA l)
deriving instance (Ord l) ⇒ Ord (ValA l)
deriving instance (Show l) ⇒ Show (ValA l)

instance (Fmt l) ⇒ Fmt (ValA l) where
    fmt = \case
        TopNum → fmt "TopNum"
        TopTxt → fmt "TopTxt"
        Flat v → fmt v

lambda ∷ (Fmt l, MonadExc m) ⇒ ValA l → m (Variable, Expr, Env l)
lambda val = case val of
    Flat (LamV x e r) → ω (x, e, r)
    _ → notAFunction

op2 ∷ (Fmt l, MonadExc m, Alternative m) ⇒ Op2 → ValA l → ValA l → m (ValA l)
op2 o a b = case (o, a, b) of
    (Divide, _, Flat (NumV n)) | isNumeric a, n == 0 → divisionByZero
    (Divide, _, TopNum) | isNumeric a → ω TopNum ⫶ divisionByZero
    (Plus, _, _) | isTxt a → ω TopTxt
    _ | isNumeric a, isNumeric b → ω TopNum
    _ → invalidArgsToOperator
  where
    isNumeric = \case TopNum → True; Flat (NumV _) → True; _ → False
    isTxt = \case TopTxt → True; Flat (TxtV _) → True; _ → False

if' ∷ (MonadExc m, Alternative m) ⇒ ValA l → m (ValA l) → m (ValA l) → m (ValA l)
if' tst then' else' = case tst of
    Flat (NumV n) | n == 0 → else' | otherwise → then'
    TopNum → else' ⫶ then'
    _ → branchOnNonNumeric
