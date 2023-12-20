{-# LANGUAGE BlockArguments #-}

module Stanly.Store (StoreT, runStoreT, Store, len, pruneₛ, lookupᵥ, gc, store') where

import Control.Monad.Except (MonadError (throwError))
import Control.Monad.State (MonadState (get), StateT, modify, runStateT)
import Data.Char (toLower)
import Data.Coerce (coerce)
import Stanly.Fmt (Fmt (..), FmtCmd (Dim, Yellow), bwText, (⊹))
import Stanly.Unicode
import Stanly.Val (Val, pruneᵥ, regionᵥ)

newtype Store l where Store ∷ [(l, Val l)] → Store l

type StoreT l m = StateT (Store l) m
runStoreT ∷ StoreT l m a → m (a, Store l)
runStoreT = flip runStateT (Store [])

len ∷ Store l → Int
len (Store σ) = length σ

pruneₛ ∷ (Eq l) ⇒ (l → Bool) → Store l → Store l
pruneₛ p (Store σ) = Store [(l, pruneᵥ val) | (l, val) ← σ, p l]

gc ∷ (Eq l) ⇒ Val l → Store l → Store l
gc val = pruneₛ (∈ regionᵥ (pruneᵥ val))

lookupᵥ ∷ (Fmt l, Eq l, MonadError String m, MonadState (Store l) m) ⇒ l → m (Val l)
lookupᵥ l =
    get ⇉ \(Store s) → case lookup l s of
        Just x → ω x
        Nothing → throwError ⎴ bwText ⎴ " not found in store.\n" ⊹ Store s

store' ∷ (MonadState (Store l) m) ⇒ (l, Val l) → m ()
store' (l, v) = modify ⎴ coerce \σ → (l, v) : σ

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
