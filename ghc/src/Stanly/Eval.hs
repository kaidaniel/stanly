module Stanly.Eval (
    eval,
    trace,
    dead,
    Trace (..),
    Val (..),
    Env (..),
    Loc,
    Exception (..),
    Interpreter (..),
    MonadExc,
    MonadEnv,
    (.>),
    mix,
) where

import Control.Monad.Except qualified as M
import Control.Monad.Reader qualified as M
import Control.Monad.State qualified as M
import Control.Monad.Writer qualified as M
import Data.Set qualified as Set
import Stanly.Language (Expr (..), Op2, Variable, subexprs)
import Stanly.Unicode

type Loc = String

newtype Env = MkEnv [(Variable, Loc)]
    deriving (Eq, Ord, Semigroup, Monoid, Show)

data Val
    = LamV Variable Expr Env
    | NumV Integer
    | TxtV String
    | AnyV
    deriving (Eq, Ord, Show)

type MonadExc = M.MonadError Exception
type MonadEnv = M.MonadReader Env

class (MonadExc m, MonadEnv m) ⇒ Interpreter m where
    op2 ∷ Op2 → Val → Val → m Val
    isTruthy ∷ Val → m Bool
    find ∷ Loc → m Val
    ext ∷ Loc → Val → m ()
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
    If' tst then' else' → do
        t ← call tst
        b ← isTruthy t
        if b then call then' else call else'
    Op2 o e₁ e₂ → do
        lhs ← call e₁
        rhs ← call e₂
        op2 o lhs rhs
    Rec var body → do
        ρ ← M.ask
        loc ← alloc var
        val ← M.local (\_ → γ [(var, loc)] <> ρ) (call body)
        ext loc val
        ω val
    App f x → do
        f₁ ← call f
        case f₁ of
            LamV var body ρ → do
                x₁ ← call x
                loc ← alloc var
                ext loc x₁
                M.local (\_ → γ [(var, loc)] <> ρ) (call body)
            _ → M.throwError ⎴ NotAFunction (App f x) f₁

data Exception
    = DivisionByZero {lhs ∷ Val, rhs ∷ Val}
    | InvalidArgsToOperator {lhs ∷ Val, op ∷ Op2, rhs ∷ Val}
    | BranchOnNonNumeric {val ∷ Val}
    | InvalidLoc {loc ∷ Loc}
    | UndefinedName {expr ∷ Expr, env ∷ Env}
    | NotAFunction {expr ∷ Expr, val ∷ Val}
    | Exception
    deriving (Eq, Ord, Show)

instance Semigroup Exception where
    a <> _ = a

instance Monoid Exception where
    mempty = Exception

type Mixin s = s → s → s

(.>) ∷ Mixin s → Mixin s → Mixin s
f .> g = \cc call → f (g cc call) call

mix ∷ Mixin s → s
mix f = let m = mix f in f m m

newtype Trace a = Trace [(Expr, Env, a)] deriving (Eq, Show, Semigroup, Monoid)

{-# INLINEABLE trace #-}
trace ∷
    ∀ a m.
    (M.MonadState a m, M.MonadWriter (Trace a) m, Interpreter m) ⇒
    Mixin (Expr → m Val)
trace cc _ = \expr → do
    r ← M.ask
    s ← M.get
    M.tell (Trace [(expr, r, s)])
    cc expr

dead ∷ Trace a → Expr → Set.Set Expr
dead (Trace t) ast = Set.fromList (withoutSubExprs (Set.toList notVisited))
  where
    visited = Set.fromList (map (\(e, _, _) → e) t)
    allExprs = Set.fromList (subexprs ast)
    withoutSubExprs li = [x | x ← li, x `notElem` (li >>= subexprs)]
    notVisited = Set.difference allExprs visited
