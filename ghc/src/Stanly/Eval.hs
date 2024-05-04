module Stanly.Eval (
    eval,
    Val (..),
    Env (..),
    Exception (..),
    Store (..),
    Interpreter (..),
) where

import Control.Monad.Except qualified as M
import Control.Monad.Reader qualified as M
import Control.Monad.State qualified as M
import Data.Coerce
import Data.Either
import Data.Map qualified as Map
import Stanly.Language (Expr (..), Op2, Variable)
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

class (M.MonadError Exception m, M.MonadReader Env m, M.MonadState Store m) ⇒ Interpreter m where
    op2 ∷ Op2 → Val → Val → m Val
    if' ∷ m Val → m Val → m Val → m Val
    find ∷ Loc → m Val
    alloc ∷ Variable → m Loc

eval ∷ (Interpreter m) ⇒ (Expr → m Val) → (Expr → m Val)
eval eval₁ = \case
    Num n → ω ⎴ NumV n
    Txt s → ω ⎴ TxtV s
    Lam x e → φ (LamV x e) M.ask
    Var x → do
        ρ ← M.ask
        case lookup x (coerce ρ) of
            Just loc → find loc
            Nothing → M.throwError (UndefinedName (Var x) ρ)
    If' tst then' else' → if' (eval₁ tst) (eval₁ then') (eval₁ else')
    Op2 o e₁ e₂ → do
        lhs ← eval₁ e₁
        rhs ← eval₁ e₂
        op2 o lhs rhs
    Rec var body → do
        ρ ← M.ask
        loc ← alloc var
        val ← M.local (\_ → insertEnv (var, loc) ρ) (eval₁ body)
        M.modify ⎴ coerce (\s → Map.insert loc val s)
        ω val
    App f x → do
        f₁ ← eval₁ f
        case f₁ of
            LamV var body ρ → do
                x₁ ← eval₁ x
                loc ← alloc var
                M.modify ⎴ coerce (\s → Map.insert loc x₁ s)
                M.local (\_ → insertEnv (var, loc) ρ) (eval₁ body)
            _ → M.throwError (NotAFunction (App f x) f₁)
  where
    insertEnv binding (Env ρ) = Env (binding : ρ)

newtype Store = Store (Map.Map Loc Val)
    deriving (Eq, Semigroup, Monoid, Show)

data Exception
    = DivisionByZero {lhs ∷ Val, rhs ∷ Val}
    | InvalidArgsToOperator {lhs ∷ Val, op ∷ Op2, rhs ∷ Val}
    | BranchOnNonNumeric {val ∷ Val}
    | InvalidLoc {loc ∷ Loc, values ∷ Store}
    | UndefinedName {expr ∷ Expr, environment ∷ Env}
    | NotAFunction {expr ∷ Expr, fval ∷ Val}
    deriving (Eq, Show)
