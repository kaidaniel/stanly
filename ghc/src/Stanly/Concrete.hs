{-# LANGUAGE ScopedTypeVariables #-}

module Stanly.Concrete (runConcrete) where

import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.Reader (MonadReader (ask, local), ReaderT (runReaderT))
import Control.Monad.State (MonadState (get), StateT (runStateT), gets, modify)
import Data.Coerce (coerce)
import Data.Maybe (fromMaybe)
import Stanly.Fmt (bwText, (⊹))
import Stanly.Interpreter (Eval, Interpreter (..), arithmetic, branchIfn0)
import Stanly.Language (Expr)
import Stanly.MachineState (Env (..), Store (..), Val)
import Stanly.Unicode

type EnvT m = ReaderT (Env Int) m
type StoreT m = StateT (Store Int) m
type ExcT m = ExceptT String m

type ConcreteT m = ExcT (EnvT (StoreT m))

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
                    Just l → get ⇉ ω ∘ \store → (error ⎴ bwText ⎴ " not found in store.\n" ⊹ store) `fromMaybe` (lookup l ⎴ coerce store)
                    Nothing → throwError (show var ⋄ " not found in environment: " ⋄ bwText (Env ρ))
            , closure = (`φ` ask)
            , bind = \binding cc → local (Env [binding] ⋄) ⎴ cc
            , alloc = const (gets length)
            , substitute = \ρ₁ binding cc → local (const (Env [binding] ⋄ ρ₁)) ⎴ cc
            , storeₗ = \binding → modify (coerce [binding] ⋄)
            , op2 = arithmetic
            , branch = branchIfn0
            }
