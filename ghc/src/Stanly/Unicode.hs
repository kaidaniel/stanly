{-# LANGUAGE StarIsType #-}

module Stanly.Unicode where

import Control.Applicative (Alternative (..), empty)
import Control.Arrow ((>>>))
import Control.Category (Category)
import Control.Monad (MonadPlus, mzero)

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

(<**>) ∷ (Applicative f) ⇒ f (α → β) → f α → f β
(<**>) = (<*>)
{-# INLINE (<**>) #-}

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

(⎴) ∷ (a → b) → a → b
(⎴) = ($)
infixr 0 ⎴
{-# INLINE (⎴) #-}

(⫿) ∷ (Alternative f) ⇒ f a → f a → f a
(⫿) = (<|>)
{-# INLINE (⫿) #-}
infixl 3 ⫿

(⋄) ∷ (Semigroup a) ⇒ a → a → a
(⋄) = (<>)
{-# INLINE (⋄) #-}
infixr 6 ⋄
