module Stanly.Val.Concrete (
    lambda,
    op2,
    if',
) where

import Stanly.Env (Env)
import Stanly.Exc (
    MonadExc,
    branchOnNonNumeric,
    divisionByZero,
    invalidArgsToOperator,
    notAFunction,
 )
import Stanly.Fmt (Fmt)
import Stanly.Language (Expr, Op2 (..), Variable)
import Stanly.Unicode
import Stanly.Val.Internal (Val (..))

lambda ∷ (Fmt l, MonadExc m) ⇒ Val l → m (Variable, Expr, Env l)
lambda val = case val of
    LamV x e r → ω (x, e, r)
    _ → notAFunction

op2 ∷ (Fmt l, MonadExc m) ⇒ Op2 → Val l → Val l → m (Val l)
op2 o a b = do
    case (a, b) of
        (NumV n₀, NumV n₁)
            | o == Plus → ω ⎴ NumV ⎴ n₀ + n₁
            | o == Minus → ω ⎴ NumV ⎴ n₀ - n₁
            | o == Times → ω ⎴ NumV ⎴ n₀ * n₁
            | o == Divide, n₁ == 0 → divisionByZero
            | o == Divide → ω ⎴ NumV ⎴ div n₀ n₁
        (TxtV t₀, TxtV t₁)
            | o == Plus → ω ⎴ TxtV ⎴ t₀ ⋄ t₁
        (TxtV t₀, NumV n₁)
            | o == Plus → ω ⎴ TxtV ⎴ t₀ ⋄ show n₁
        _ → invalidArgsToOperator

if' ∷ (MonadExc m) ⇒ Val l → m (Val l) → m (Val l) → m (Val l)
if' tst then' else' = case tst of
    NumV n | n == 0 → else' | otherwise → then'
    _ → branchOnNonNumeric
