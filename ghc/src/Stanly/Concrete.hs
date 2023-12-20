{-# LANGUAGE ScopedTypeVariables #-}

module Stanly.Concrete (runConcrete, Store, pruneₛ, gc) where

import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.Reader (MonadReader (ask, local), ReaderT (runReaderT))
import Control.Monad.State (MonadState (get), StateT (runStateT), gets, modify)
import Data.Char (toLower)
import Data.Coerce (coerce)
import Stanly.Env (Env (..), Val (..), pruneᵥ, regionᵥ)
import Stanly.Fmt (Fmt (..), FmtCmd (Dim, Yellow), bwText, (⊹))
import Stanly.Interpreter (Eval, Interpreter (..), arithmetic, branchIfn0)
import Stanly.Language (Expr)
import Stanly.Unicode

type EnvT m = ReaderT (Env Int) m
type StoreT m = StateT (Store Int) m
type ExcT m = ExceptT String m

type ConcreteT m = ExcT (EnvT (StoreT m))

newtype Store l where
    Store ∷ [(l, Val l)] → Store l
    deriving (Semigroup, Monoid)

len ∷ Store l → Int
len (Store σ) = length σ

pruneₛ ∷ (Eq l) ⇒ (l → Bool) → Store l → Store l
pruneₛ p (Store σ) = Store [(l, pruneᵥ val) | (l, val) ← σ, p l]

gc ∷ (Eq l) ⇒ Val l → Store l → Store l
gc val = pruneₛ (∈ regionᵥ (pruneᵥ val))

runConcrete ∷ ∀ m. (Monad m) ⇒ (Interpreter Int (ConcreteT m) → Eval Int (ConcreteT m)) → Expr → m (Either String (Val Int), Store Int)
runConcrete ev = rstore ∘ renv ∘ rexc ∘ ev₁
  where
    rstore = flip runStateT ε₁
    renv = flip runReaderT ε₁
    rexc = runExceptT
    ev₁ = ev concreteInterpreter
    concreteInterpreter =
        Interpreter
            { load = \var →
                ask ⇉ \(Env ρ) → case lookup var ρ of
                    Just l →
                        get ⇉ \(Store store) → case lookup l ⎴ store of
                            Just x → ω x
                            Nothing → throwError ⎴ bwText ⎴ " not found in store.\n" ⊹ Store store
                    Nothing → throwError (show var ⋄ " not found in environment: " ⋄ bwText (Env ρ))
            , closure = (`φ` ask)
            , bind = \binding cc → local (Env [binding] ⋄) ⎴ cc
            , alloc = const (gets len)
            , substitute = \ρ₁ binding cc → local (const (Env [binding] ⋄ ρ₁)) ⎴ cc
            , storeₗ = \binding → modify (coerce [binding] ⋄)
            , op2 = arithmetic
            , branch = branchIfn0
            }

instance (Fmt l) ⇒ Fmt (Store l) where
    fmt (Store σ) = case reverse σ of
        [] → ε₁
        (x : xs) → line x ⊹ ["\n" ⊹ line x₁ | x₁ ← xs]
      where
        prefix x = Dim ⊹ [toLower c | c ← take 3 (show x)] ⊹ " "
        line (k, v) = (Dim ⊹ "stor ") ⊹ (Yellow ⊹ (padded ⎴ bwText k) ⊹ " ") ⊹ prefix v ⊹ v
          where
            padded = \case
                [] → "    "
                s@[_] → "   " ⋄ s
                s@[_, _] → "  " ⋄ s
                s@[_, _, _] → " " ⋄ s
                s → s
