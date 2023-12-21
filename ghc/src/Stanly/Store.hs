{-# LANGUAGE BlockArguments #-}

module Stanly.Store (StoreT, runStoreT, Store, len, pruneₛ, lookupᵥ, store') where

import Control.Monad.Except (MonadError (throwError))
import Control.Monad.State (MonadState (get), StateT, modify, runStateT)
import Data.Bifunctor (Bifunctor (..))
import Data.Char (toLower)
import Data.Coerce (coerce)
import Stanly.Fmt (Fmt (..), FmtCmd (Dim, Yellow), bwText, (⊹))
import Stanly.Unicode
import Stanly.Val (Val)
import Data.IntMap (IntMap, insert, lookup, size)

newtype Store l v where
    Store ∷ {unStore ∷ [(l, v)]} → Store l v
    deriving (Eq, Ord, Semigroup, Monoid)

type StoreT l m = StateT (Store l (Val l)) m

runStoreT ∷ StoreT l m a → m (a, Store l (Val l))
runStoreT = flip runStateT ε₁

len ∷ Store l v → Int
len (Store σ) = length σ

pruneₛ ∷ (Eq l) ⇒ (l → Bool) → Store l v → Store l v
pruneₛ predicate (Store σ) = Store [(l, val) | (l, val) ← σ, predicate l]

lookupᵥ ∷ (Show v, Fmt v, Fmt l, Eq l, MonadError String m, MonadState (Store l v) m) ⇒ l → m v
lookupᵥ l =
    get ⇉ \(Store s) → case lookup l s of
        Just x → ω x
        Nothing → throwError ⎴ bwText ⎴ " not found in store.\n" ⊹ Store s

store' ∷ (MonadState (Store l v) m) ⇒ (l, v) → m ()
store' (l, v) = modify ⎴ coerce \σ → (l, v) : σ

instance (Show v, Fmt v, Fmt l) ⇒ Fmt (Store l v) where
    fmt σ = foldMap line (reverse ⎴ unStore ⎴ bimap f₁ f₂ σ)
      where
        f₁ loc = (Dim ⊹ "stor ") ⊹ (Yellow ⊹ (padded ⎴ bwText loc))
        f₂ val = Dim ⊹ [toLower c | c ← take 3 (show val)] ⊹ " " ⊹ val
        line (l, v) = "\n" ⊹ l ⊹ " " ⊹ v
        padded = \case
            [] → "    "
            s@[_] → "   " ⋄ s
            s@[_, _] → "  " ⋄ s
            s@[_, _, _] → " " ⋄ s
            s → s

instance Functor (Store l) where fmap f (Store σ) = Store [(l, f v) | (l, v) ← σ]
instance Swap Store where swap (Store σ) = Store ⎴ φ swap σ
instance Bifunctor Store where second = φ; first f σ = swap ⎴ φ f (swap σ)
class Swap p where swap ∷ p a b → p b a
instance Swap (,) where swap (a, b) = (b, a)

-- instance (Eq l, Monoid l) ⇒ Applicative (Store l) where
--     pure x = Store [(ε₁, x)]
--     Store f <*> Store x = Store [(l₁, v₁ v₂) | (l₁, v₁) ← f, (l₂, v₂) ← x, l₁ == l₂]

-- instance (Eq l, Monoid l) ⇒ Monad (Store l) where
--     return = pure
--     Store x >>= f = Store [(l, v₁) | (l, v) ← x, let Store s = f v, (l₁, v₁) ← s, l == l₁]
