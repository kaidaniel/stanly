module Stanly.Store (StoreT, len, lookupStore, insertStore, runStoreT, value, store, StoreRes) where

import Control.Monad.State (MonadState (get), StateT, modify, runStateT)
import Data.Bifunctor (Bifunctor (..))
import Data.Char (toLower)
import Data.Coerce
import Data.List (intersperse)
import Data.Map (Map, insert, size, toAscList, (!?))
import Stanly.Exc (MonadExc, varNotFoundInStore)
import Stanly.Fmt (Fmt (..), FmtCmd (Dim, Yellow), bwText, (⊹))
import Stanly.Unicode

newtype Store l v where
    Store ∷ Map l v → Store l v
    deriving (Eq, Ord, Semigroup, Monoid, Functor)

type StoreT l v m = StateT (Store l v) m

type MonadStore l v m = (MonadState (Store l v) m, Ord l, Fmt l)

newtype StoreRes l v a = StoreRes (a, Store l v) deriving (Eq, Ord)

store ∷ StoreRes l v a → Store l v
store (StoreRes x) = π₂ x

value ∷ StoreRes l v a → a
value (StoreRes x) = π₁ x

runStoreT ∷ ∀ l v m a. (Ord l, Monad m) ⇒ StoreT l v m a → m (StoreRes l v a)
runStoreT = φ StoreRes ∘ flip runStateT ε₁

len ∷ Store l v → Int
len (Store σ) = size σ

lookupStore ∷ (Show v, Fmt v, MonadExc m, MonadStore l v m) ⇒ l → m v
lookupStore l =
    get ⇉ \(Store s) → case s !? l of
        Just x → ω x
        Nothing → varNotFoundInStore

insertStore ∷ (MonadStore l v m) ⇒ (l, v) → m ()
insertStore (l, v) = modify ⎴ coerce ⎴ insert l v

instance (Ord l, Fmt l, Show v, Fmt v) ⇒ Fmt (Store l v) where
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
