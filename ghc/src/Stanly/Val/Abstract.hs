module Stanly.Val.Abstract (
    lambda,
    op2,
    if',
    ValA
) where

import Stanly.Val.Internal(Val(..), Value(..))
import Stanly.Env (Env)
import Stanly.Exc (MonadExc, notAFunction)
import Stanly.Fmt (Fmt(..))
import Stanly.Language (Expr, Op2 (..), Variable)
import Stanly.Unicode

data ValA l where
    Top :: ValA l
    Flat :: Val l -> ValA l

instance Value ValA where 
    fromVal v = Flat v

deriving instance Eq l => Eq (ValA l)
deriving instance Ord l => Ord (ValA l)
deriving instance Show l => Show (ValA l)

instance Fmt l => Fmt (ValA l) where
    fmt = \case
        Top -> fmt "Top"
        Flat v -> fmt v

lambda ∷ (Fmt l, MonadExc m) ⇒ ValA l → m (Variable, Expr, Env l)
lambda val = case val of
    Flat (LamV x e r) → ω (x, e, r)
    _ → notAFunction

op2 ∷ (Fmt l, MonadExc m) ⇒ Op2 → ValA l → ValA l → m (ValA l)
op2 o a b = undefined

if' ∷ (MonadExc m) ⇒ ValA l → m (ValA l) → m (ValA l) → m (ValA l)
if' tst then' else' = undefined
