{-# LANGUAGE StarIsType #-}

module Stanly.Unicode where

import Control.Applicative (Alternative (..), empty)
import Control.Arrow ((>>>))
import Control.Category (Category)
import Control.Monad (MonadPlus, mzero)

-- ₁ ₂ ₃ ₄ ₅ ₆ ₇ ₈ ₉ ₀

-- no-haskell-unicode
(∘) ∷ (β → γ) → (α → β) → α → γ
(∘) = (.)
{-# INLINE (∘) #-}
infixr 9 ∘

(⋙) ∷ (Category cat) ⇒ cat a b → cat b c → cat a c
(⋙) = (>>>)
{-# INLINE (⋙) #-}
infixr 1 ⋙

(≫) ∷ ∀ (m ∷ ★ → ★) α β. (Monad m) ⇒ m α → m β → m β
(≫) = (>>)
{-# INLINE (≫) #-}
infixl 1 ≫

(⊛) ∷ (Applicative f) ⇒ f (α → β) → f α → f β
(⊛) = (<*>)
{-# INLINE (⊛) #-}
infixl 4 ⊛

(<*>!) ∷ (Applicative f) ⇒ f (α → β) → f α → f β
(<*>!) = (<*>)
{-# INLINE (<*>!) #-}

εₐ ∷ (Alternative f) ⇒ f α
εₐ = empty
{-# INLINE εₐ #-}

εₘ ∷ (MonadPlus m) ⇒ m a
εₘ = mzero
{-# INLINE εₘ #-}

ε₁ ∷ (Monoid m) ⇒ m
ε₁ = mempty
{-# INLINE ε₁ #-}

ω ∷ (Applicative m) ⇒ a → m a
ω = pure
{-# INLINE ω #-}
fmap_, φ ∷ (Functor f) ⇒ (a → b) → f a → f b
φ = fmap
{-# INLINE φ #-}
fmap_ = fmap
{-# INLINE fmap_ #-}
κ₁ ∷ (Monoid m) ⇒ [m] → m
κ₁ = mconcat
{-# INLINE κ₁ #-}
κₗ ∷ (Foldable t, Monoid m) ⇒ t [m] → [m]
κₗ = concat
κλ ∷ (Foldable t) ⇒ (a → [b]) → t a → [b]
κλ = concatMap
{-# INLINE κλ #-}

(⎴) ∷ (a → b) → a → b
(⎴) = ($)
infixr 0 ⎴
{-# INLINE (⎴) #-}

(⫶) ∷ (Alternative f) ⇒ f a → f a → f a
(⫶) = (<|>)
{-# INLINE (⫶) #-}
infixl 3 ⫶

(⋄) ∷ (Semigroup a) ⇒ a → a → a
(⋄) = (<>)
{-# INLINE (⋄) #-}
infixr 6 ⋄

(⇉) ∷ (Monad m) ⇒ m a → (a → m b) → m b
(⇉) = (>>=)
{-# INLINE (⇉) #-}
infixl 1 ⇉

(>>=!) ∷ (Monad m) ⇒ m a → (a → m b) → m b
(>>=!) = (>>=)
{-# INLINE (>>=!) #-}
infixl 1 >>=!

π₁ ∷ ∀ {a} {b}. (a, b) → a
π₁ = fst
{-# INLINE π₁ #-}

π₂ ∷ ∀ {a} {b}. (a, b) → b
π₂ = snd
{-# INLINE π₂ #-}

π₃₁ ∷ ∀ {a} {b} {c}. (a, b, c) → a
π₃₁ (a, _, _) = a
{-# INLINE π₃₁ #-}

π₃₂ ∷ ∀ {a} {b} {c}. (a, b, c) → b
π₃₂ (_, b, _) = b
{-# INLINE π₃₂ #-}

π₃₃ ∷ ∀ {a} {b} {c}. (a, b, c) → c
π₃₃ (_, _, c) = c
{-# INLINE π₃₃ #-}
