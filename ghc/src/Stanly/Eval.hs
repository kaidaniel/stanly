{-# LANGUAGE RecursiveDo #-}

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
    prune,
    Mixin,
    cached,
    Cache,
    Cached,
) where

import Control.Applicative qualified as M
import Control.Monad.Except qualified as M
import Control.Monad.Fix qualified as M
import Control.Monad.Reader qualified as M
import Control.Monad.State qualified as M
import Control.Monad.Writer qualified as M
import Data.Map qualified as Map
import Data.Set qualified as Set
import Stanly.Language qualified as L
import Stanly.Unicode

type Loc = String

newtype Env = MkEnv [(L.Variable, Loc)]
    deriving (Eq, Ord, Semigroup, Monoid, Show)

data Val
    = Lam L.Variable L.Expr Env
    | Num Integer
    | Txt String
    | Any
    deriving (Eq, Ord, Show)

prune ∷ Val → Val
prune = \case
    Lam x expr (MkEnv env) → Lam x expr (MkEnv [(v, l) | (v, l) ← env, v `Set.member` L.freeVars (L.Lam x expr)])
    x → x

type MonadExc = M.MonadError Exception
type MonadEnv = M.MonadReader Env

class Interpreter m where
    op2 ∷ L.Op2 → Val → Val → m Val
    isTruthy ∷ Val → m Bool
    find ∷ Loc → m Val
    ext ∷ Loc → Val → m ()
    alloc ∷ L.Variable → m Loc

eval ∷ (MonadExc m, MonadEnv m, Interpreter m) ⇒ Mixin (L.Expr → m Val)
eval _ call = \case
    L.Num n → ω ⎴ Num n
    L.Txt s → ω ⎴ Txt s
    L.Lam x e → φ (Lam x e) M.ask
    L.Any → ω Any
    L.Var x → do
        ρ ← M.ask
        case lookup x (γ ρ) of
            Just loc → find loc
            Nothing → M.throwError ⎴ UndefinedName (L.Var x) ρ
    L.If' tst then' else' → do
        t ← call tst
        b ← isTruthy t
        if b then call then' else call else'
    L.Op2 o e₁ e₂ → do
        lhs ← call e₁
        rhs ← call e₂
        op2 o lhs rhs
    L.Rec var body → do
        ρ ← M.ask
        loc ← alloc var
        val ← M.local (\_ → γ [(var, loc)] <> ρ) (call body)
        ext loc val
        ω val
    L.App f x → do
        f₁ ← call f
        case f₁ of
            Lam var body ρ → do
                x₁ ← call x
                loc ← alloc var
                ext loc x₁
                M.local (\_ → γ [(var, loc)] <> ρ) (call body)
            _ → M.throwError ⎴ NotAFunction (L.App f x) f₁

data Exception
    = DivisionByZero {lhs ∷ Val, rhs ∷ Val}
    | InvalidArgsToOperator {lhs ∷ Val, op ∷ L.Op2, rhs ∷ Val}
    | BranchOnNonNumeric {val ∷ Val}
    | InvalidLoc {loc ∷ Loc}
    | UndefinedName {expr ∷ L.Expr, env ∷ Env}
    | NotAFunction {expr ∷ L.Expr, val ∷ Val}
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

newtype Trace a = Trace [(L.Expr, Env, a)] deriving (Eq, Show, Semigroup, Monoid)

{-# INLINEABLE trace #-}
trace ∷
    ∀ a m.
    (M.MonadState a m, M.MonadWriter (Trace a) m, MonadEnv m) ⇒
    Mixin (L.Expr → m Val)
trace cc _ = \expr → do
    r ← M.ask
    s ← M.get
    M.tell (Trace [(expr, r, s)])
    cc expr

dead ∷ Trace a → L.Expr → Set.Set L.Expr
dead (Trace t) ast = Set.fromList (withoutSubExprs (Set.toList notVisited))
  where
    visited = Set.fromList (map (\(e, _, _) → e) t)
    allExprs = Set.fromList (L.subexprs ast)
    withoutSubExprs li = [x | x ← li, x `notElem` (li >>= L.subexprs)]
    notVisited = Set.difference allExprs visited

type Cache s = Map.Map (L.Expr, Env, s) (Set.Set (Val, s))

class (Ord s) ⇒ Cached s m where
    cacheOut ∷ m (Cache s)
    cacheIn ∷ m (Cache s)
    modifyCacheOut ∷ (Cache s → Cache s) → m ()
    localCacheIn ∷ Cache s → m Val → m (Cache s)

{-# INLINEABLE cached #-}
cached ∷
    ∀ s m.
    (Cached s m, MonadEnv m, M.MonadState s m, M.MonadFix m, M.Alternative m) ⇒
    Mixin (L.Expr → m Val)
cached = fixCached .> go
  where
    fixCached cc _ = \expr → do
        env ← M.ask
        store ← M.get
        let config = (expr, env, store)
        rec cache ← do
                modifyCacheOut @s \_ → Map.empty
                M.put store
                localCacheIn @s cache (cc expr)
                cacheOut
        case (cache Map.!? config) of
            Just vXs's → foldCache vXs's
            Nothing → M.empty
    go cc _ = \expr → do
        env ← M.ask
        store ← M.get
        let config = (expr, env, store)
        out ← cacheOut
        case out Map.!? config of
            Just vXs's → foldCache vXs's
            Nothing → do
                in' ← cacheIn
                modifyCacheOut \_ → Map.insert config (Map.findWithDefault Set.empty config in') out
                v ← cc expr
                store' ← M.get
                modifyCacheOut ⎴ Map.adjust (<> Set.singleton (v, store')) config
                pure v
    foldCache = \vXs's → Set.foldl' (\m (v, s) → do M.put s; m M.<|> pure v) M.empty vXs's
