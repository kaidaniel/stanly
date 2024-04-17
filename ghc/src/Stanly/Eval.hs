module Stanly.Eval (runConcrete, eval, store) where

import Control.Monad (ap)
import Control.Monad.Except (MonadError (..))
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.Trans (MonadTrans (..))
import Data.Bifunctor (Bifunctor (bimap))
import Data.Char (toLower)
import Data.Coerce
import Data.List (intersperse)
import Data.Map (Map, insert, size, toAscList, (!?))
import Stanly.Fmt (Fmt (..), FmtCmd (..), bwText, (⊹))
import Stanly.Language (Expr (..), Op2 (..), Variable)
import Stanly.Unicode

type Loc = Int

newtype Env where
    Env ∷ [(Variable, Loc)] → Env
    deriving (Eq, Ord, Semigroup, Monoid, Show)

insertEnv ∷ (Variable, Loc) → Env → Env
insertEnv binding (Env ρ) = Env (binding : ρ)

data Val
    = LamV Variable Expr Env
    | NumV Integer
    | TxtV String
    deriving (Eq)

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
            Nothing → throwError (UndefinedName x ρ)
    If' tst then' else' → if' (eval₁ tst) (eval₁ then') (eval₁ else')
    Op2 o e₁ e₂ → do
        lhs ← eval₁ e₁
        rhs ← eval₁ e₂
        op2 o lhs rhs
    Rec var body → do
        ρ ← env
        loc ← alloc var
        value ← inEnv (insertEnv (var, loc) ρ) (eval₁ body)
        update (loc, value)
        ω value
    App f x → do
        f₁ ← eval₁ f
        case f₁ of
            LamV var body ρ → do
                x₁ ← eval₁ x
                loc ← alloc var
                update (loc, x₁)
                inEnv (insertEnv (var, loc) ρ) (eval₁ body)
            _ → throwError (NotAFunction f f₁)

newtype Store = Store (Map Loc Val)
    deriving (Eq, Semigroup, Monoid, Show)
data Exception
    = DivisionByZero Integer Integer
    | InvalidArgsToOperator Val Op2 Val
    | BranchOnNonNumeric Val
    | InvalidLoc Loc Store
    | UndefinedName Variable Env
    | NotAFunction Expr Val
    deriving (Eq, Show)
data Res a = Step a Store | Stop Exception deriving (Functor)
newtype InterpreterT m a = InterpreterT (Env → Store → m (Res a)) deriving (Functor)

instance (Monad m) ⇒ Applicative (InterpreterT m) where
    pure x = InterpreterT \_ s → pure ⎴ Step x s
    (<*>) = ap

instance (Monad m) ⇒ Monad (InterpreterT m) where
    (InterpreterT m₁) >>= f = InterpreterT \e s₁ → do
        res ← m₁ e s₁
        case res of
            Step a s₂ → let InterpreterT m₂ = f a in m₂ e s₂
            Stop exc → ω (Stop exc)

instance (Monad m) ⇒ MonadError Exception (InterpreterT m) where
    throwError exc = InterpreterT \_ _ → ω (Stop exc)
    catchError (InterpreterT m₁) f = InterpreterT \e s₁ → do
        res ← m₁ e s₁
        case res of
            Step a s₂ → ω (Step a s₂)
            Stop exc → let InterpreterT m₂ = f exc in m₂ e s₁

instance MonadTrans InterpreterT where
    lift m = InterpreterT \_ s → do
        v ← m
        ω ⎴ Step v s

newtype ConcreteT m a = ConcreteT (InterpreterT m a)
    deriving (Functor, Applicative, Monad, MonadError Exception, MonadTrans)

store ∷ ConcreteT Identity a → Store
store (ConcreteT (InterpreterT f)) = case runIdentity (f ε₁ ε₁) of
    Step _ s → s
    Stop _ → ε₁

runConcrete ∷ ConcreteT Identity a → Res a
runConcrete (ConcreteT (InterpreterT f)) = runIdentity (f ε₁ ε₁)

instance (Monad m) ⇒ Interpreter (ConcreteT m) where
    op2 o lhs rhs = do
        case (lhs, rhs) of
            (NumV n₀, NumV n₁)
                | o == Plus → ω ⎴ NumV ⎴ n₀ + n₁
                | o == Minus → ω ⎴ NumV ⎴ n₀ - n₁
                | o == Times → ω ⎴ NumV ⎴ n₀ * n₁
                | o == Divide, n₁ == 0 → throwError (DivisionByZero n₀ n₁)
                | o == Divide → ω ⎴ NumV ⎴ div n₀ n₁
            (TxtV t₀, TxtV t₁)
                | o == Plus → ω ⎴ TxtV ⎴ t₀ ⋄ t₁
            (TxtV t₀, NumV n₁)
                | o == Plus → ω ⎴ TxtV ⎴ t₀ ⋄ show n₁
            _ → throwError (InvalidArgsToOperator lhs o rhs)
    if' tst then' else' = do
        tst' ← tst
        case tst' of
            NumV n | n == 0 → else' | otherwise → then'
            _ → throwError (BranchOnNonNumeric tst')
    env = γ \ρ s → ω @m (Step (Env ρ) s)
    inEnv ρ = γ \m₁ (_ ∷ Env) s → m₁ ρ (Store s) ∷ m (Res Val)
    update (loc, val) = γ \(_ ∷ Env) s → ω @m (Step () (Store ⎴ insert loc val s))
    find loc = γ \(_ ∷ Env) s → case s !? loc of
        Just v → ω @m (Step v (Store s))
        Nothing → ω @m (Stop ⎴ InvalidLoc loc (Store s))
    alloc _ = γ \(_ ∷ Env) s → ω @m (Step (size s) (Store s))

instance Fmt Exception where
    fmt e = fmt (show e)

instance (Fmt a) ⇒ Fmt (Res a) where
    fmt (Step a _) = fmt a
    fmt (Stop exc) = fmt exc

instance Fmt Store where
    fmt (Store σ) = κ₁ ⎴ intersperse (fmt '\n') items
      where
        f₁ loc = (Dim ⊹ "store ") ⊹ (Yellow ⊹ (padded ⎴ bwText loc))
        f₂ val = Dim ⊹ [toLower c | c ← take 3 (show val)] ⊹ " " ⊹ val
        items = φ (\(l, r) → l ⊹ ' ' ⊹ r) ⎴ φ (bimap f₁ f₂) (toAscList σ)
        padded = \case
            [] → "    "
            s@[_] → "   " ⋄ s
            s@[_, _] → "  " ⋄ s
            s@[_, _, _] → " " ⋄ s
            s → s

instance Show Val where
    show = \case
        LamV x body _ → "LamV" ⋄ " " ⋄ show x ⋄ " " ⋄ show body
        NumV n → "NumV" ⋄ " " ⋄ show n
        TxtV s → "TxtV" ⋄ " " ⋄ show s

instance Fmt Val where
    fmt = \case
        LamV x body r → "λ" ⊹ (Bold ⊹ x) ⊹ "." ⊹ body ⊹ " " ⊹ r
        NumV n → Dim ⊹ n
        TxtV s → Dim ⊹ s

instance Fmt Env where
    fmt (Env r) = (Yellow ⊹ "Γ⟦") ⊹ fmt₁ (r, "") ⊹ (Yellow ⊹ "⟧")
      where
        fmt₁ = \case
            ((v, a) : r₁, sep) → sep ⊹ v ⊹ ": " ⊹ (Yellow ⊹ a) ⊹ fmt₁ (r₁, ", ")
            ([], _) → ε₁

-- instance (Interpreter m, Monoid w) ⇒ Interpreter (WriterT w m) where
--     op2 o lhs rhs = lift (op2 o lhs rhs)
--     if' x y z = ω if' ⊛ φ ω x ⊛ φ ω y ⊛ φ ω z >>= lift
--     env = lift env
--     inEnv e m = ω (inEnv e) ⊛ φ ω m >>= lift
--     update b = lift (update b)
--     find loc = lift (find loc)
--     alloc var = lift (alloc var)
--     store = lift store

-- newtype Trace = Trace [(Expr, Env, Store)] deriving (Semigroup, Monoid)

-- trace ∷ (Interpreter m) ⇒ Expr → m Trace
-- trace =
--     execWriterT ∘ fix \ev e → do
--         ρ ← env
--         s ← store
--         tell (Trace [(e, ρ, s)])
--         eval ev e

-- newtype Dead = Dead [Expr] deriving (Semigroup, Monoid)

-- dead ∷ (Interpreter m) ⇒ Expr → m Dead
-- dead expr = do
--     Dead visited ← execWriterT (fix f expr)
--     let dead' = subexprs expr \\ visited
--     pure (Dead (dead' \\ (dead' >>= subexprs)))
--   where
--     f ev e = do
--         tell (Dead [e])
--         eval ev e

-- instance Fmt Trace where
--     fmt (Trace li) = κ₁ (φ ln (zip li [1 ∷ Integer ..]))
--       where
--         low3 e = [toLower x | x ← take 3 ⎴ show e] <> "  "
--         ln ((e, ρ, σ), i) = (Dim ⊹ i ⊹\ low3 e) ⊹ " " ⊹ (e ⊹\ ((Dim ⊹ "envir ") ⊹ ρ) ⊹\ σ) ⊹ "\n"

-- instance Fmt Dead where
--     fmt (Dead li) = case li of [] → ε₁; [x] → fmt x; (x : xs) → x ⊹\ Dead xs
