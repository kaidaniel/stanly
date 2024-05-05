module Stanly.Eval (
    eval,
    trace,
    Trace (..),
    Val (..),
    Env (..),
    Exception (..),
    Store (..),
    Interpreter (..),
    MonadExc,
    MonadEnv,
    MonadStore,
    (.>),
    mix,
) where

import Control.Monad.Except qualified as M
import Control.Monad.Reader qualified as M
import Control.Monad.State qualified as M
import Control.Monad.Writer qualified as M
import Data.Map qualified as Map
import Stanly.Language (Expr (..), Op2, Variable)
import Stanly.Unicode

type Loc = Int

newtype Env = MkEnv [(Variable, Loc)]
    deriving (Eq, Ord, Semigroup, Monoid, Show)

data Val
    = LamV Variable Expr Env
    | NumV Integer
    | TxtV String
    deriving (Eq, Show)

type MonadExc = M.MonadError Exception
type MonadEnv = M.MonadReader Env
type MonadStore = M.MonadState Store

class (MonadExc m, MonadEnv m, MonadStore m) ⇒ Interpreter m where
    op2 ∷ Op2 → Val → Val → m Val
    if' ∷ m Val → m Val → m Val → m Val
    find ∷ Loc → m Val
    alloc ∷ Variable → m Loc

eval ∷ (Interpreter m) ⇒ Mixin (Expr → m Val)
eval _ call = \case
    Num n → ω ⎴ NumV n
    Txt s → ω ⎴ TxtV s
    Lam x e → φ (LamV x e) M.ask
    Var x → do
        ρ ← M.ask
        case lookup x (γ ρ) of
            Just loc → find loc
            Nothing → M.throwError ⎴ UndefinedName (Var x) ρ
    If' tst then' else' → if' (call tst) (call then') (call else')
    Op2 o e₁ e₂ → do
        lhs ← call e₁
        rhs ← call e₂
        op2 o lhs rhs
    Rec var body → do
        ρ ← M.ask
        loc ← alloc var
        val ← M.local (\_ → γ [(var, loc)] <> ρ) (call body)
        M.modify ⎴ γ \s → Map.insert loc val s
        ω val
    App f x → do
        f₁ ← call f
        case f₁ of
            LamV var body ρ → do
                x₁ ← call x
                loc ← alloc var
                M.modify ⎴ γ \s → Map.insert loc x₁ s
                M.local (\_ → γ [(var, loc)] <> ρ) (call body)
            _ → M.throwError ⎴ NotAFunction (App f x) f₁

newtype Store = MkStore (Map.Map Loc Val)
    deriving (Eq, Semigroup, Monoid, Show)

data Exception
    = DivisionByZero {lhs ∷ Val, rhs ∷ Val}
    | InvalidArgsToOperator {lhs ∷ Val, op ∷ Op2, rhs ∷ Val}
    | BranchOnNonNumeric {val ∷ Val}
    | InvalidLoc {loc ∷ Loc, store ∷ Store}
    | UndefinedName {expr ∷ Expr, env ∷ Env}
    | NotAFunction {expr ∷ Expr, val ∷ Val}
    deriving (Eq, Show)

type Mixin s = s → s → s

(.>) ∷ Mixin s → Mixin s → Mixin s
f .> g = \cc call → f (g cc call) call

mix ∷ Mixin s → s
mix f = let m = mix f in f m m

newtype Trace = Trace [(Expr, Env, Store)] deriving (Eq, Show, Semigroup, Monoid)

{-# INLINEABLE trace #-}
trace ∷ (M.MonadWriter Trace m, Interpreter m) ⇒ Mixin (Expr → m Val)
trace cc _ = \expr → do
    r ← M.ask
    s ← M.get
    M.tell (Trace [(expr, r, s)])
    cc expr
