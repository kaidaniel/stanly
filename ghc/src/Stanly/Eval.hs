module Stanly.Eval (
    eval,
    Val (..),
    Env (..),
    Exception (..),
    Store (..),
    Interpreter (..),
    Res (..),
) where

import Control.Monad.Except (MonadError (..))
import Data.Coerce
import Data.Map (Map)
import Stanly.Language (Expr (..), Op2 (..), Variable)
import Stanly.Unicode

type Loc = Int

newtype Env where
    Env ∷ [(Variable, Loc)] → Env
    deriving (Eq, Ord, Semigroup, Monoid, Show)

data Val
    = LamV Variable Expr Env
    | NumV Integer
    | TxtV String
    deriving (Eq, Show)

class (MonadError Exception m) ⇒ Interpreter m where
    op2 ∷ Op2 → Val → Val → m Val
    if' ∷ m Val → m Val → m Val → m Val
    env ∷ m Env
    inEnv ∷ Env → m Val → m Val
    update ∷ (Loc, Val) → m ()
    find ∷ Loc → m Val
    alloc ∷ Variable → m Loc

eval ∷ (Interpreter m) ⇒ (Expr → m Val) → (Expr → m Val)
eval eval₁ = \case
    Num n → ω ⎴ NumV n
    Txt s → ω ⎴ TxtV s
    Lam x e → φ (LamV x e) env
    Var x → do
        ρ ← env
        case lookup x (coerce ρ) of
            Just loc → find loc
            Nothing → throwError (UndefinedName (Var x) ρ)
    If' tst then' else' → if' (eval₁ tst) (eval₁ then') (eval₁ else')
    Op2 o e₁ e₂ → do
        lhs ← eval₁ e₁
        rhs ← eval₁ e₂
        op2 o lhs rhs
    Rec var body → do
        ρ ← env
        loc ← alloc var
        val ← inEnv (insertEnv (var, loc) ρ) (eval₁ body)
        update (loc, val)
        ω val
    App f x → do
        f₁ ← eval₁ f
        case f₁ of
            LamV var body ρ → do
                x₁ ← eval₁ x
                loc ← alloc var
                update (loc, x₁)
                inEnv (insertEnv (var, loc) ρ) (eval₁ body)
            _ → throwError (NotAFunction (App f x) f₁)
  where
    insertEnv binding (Env ρ) = Env (binding : ρ)

newtype Store = Store (Map Loc Val)
    deriving (Eq, Semigroup, Monoid, Show)

data Exception
    = DivisionByZero {lhs ∷ Val, rhs ∷ Val}
    | InvalidArgsToOperator {lhs ∷ Val, op ∷ Op2, rhs ∷ Val}
    | BranchOnNonNumeric {val ∷ Val}
    | InvalidLoc {loc ∷ Loc, values ∷ Store}
    | UndefinedName {expr ∷ Expr, environment ∷ Env}
    | NotAFunction {expr ∷ Expr, fval ∷ Val}
    deriving (Eq, Show)
data Res a = Step a Store | Stop Exception deriving (Functor)
