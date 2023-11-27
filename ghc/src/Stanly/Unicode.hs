{-# LANGUAGE StarIsType #-}

module Stanly.Unicode (𝖏𝖋𝖋, mul, (∘), (⋙), (⋘), (≫), (∈), (∉), (∪), (∩), (⊛), (∅), 𝖟, 𝖋, 𝖋𝖋, 𝖋𝖋𝖋, 𝖕) where

import Control.Applicative (Alternative, empty, liftA2, liftA3)
import Control.Category (Category, (<<<), (>>>))
import Control.Monad (MonadPlus, join, liftM2, mzero)
import Data.List (intersect, union)

infixr 9 ∘
infixr 1 ⋙, ⋘

(∘) ∷ (β → γ) → (α → β) → α → γ
(∘) = (.)
{-# INLINE (∘) #-}

(⋙) ∷ (Category c) ⇒ c α β → c β γ → c α γ
(⋙) = (>>>)
{-# INLINE (⋙) #-}
(⋘) ∷ (Category c) ⇒ c β γ → c α β → c α γ
(⋘) = (<<<)
{-# INLINE (⋘) #-}
(≫) ∷ ∀ (m ∷ ★ → ★) α β. (Monad m) ⇒ m α → m β → m β
(≫) = (>>)
{-# INLINE (≫) #-}
(∈) ∷ (Eq α) ⇒ α → [α] → Bool
(∈) = elem
{-# INLINE (∈) #-}
(∉) ∷ (Eq α) ⇒ α → [α] → Bool
(∉) = notElem
{-# INLINE (∉) #-}
(∪) ∷ (Eq α) ⇒ [α] → [α] → [α]
(∪) = union
{-# INLINE (∪) #-}
(∩) ∷ (Eq α) ⇒ [α] → [α] → [α]
(∩) = intersect
{-# INLINE (∩) #-}
(⊛) ∷ (Applicative f) ⇒ f (α → β) → f α → f β
(⊛) = (<*>)
{-# INLINE (⊛) #-}
(∅) ∷ (Alternative f) ⇒ f α
(∅) = empty
{-# INLINE (∅) #-}
𝖋 ∷ (Functor f) ⇒ (α → β) → f α → f β
𝖋 = fmap
{-# INLINE 𝖋 #-}
𝖋𝖋 ∷ (Applicative f) ⇒ (α → β → γ) → f α → f β → f γ
𝖋𝖋 = liftA2
{-# INLINE 𝖋𝖋 #-}
𝖋𝖋𝖋 ∷ (Applicative f) ⇒ (α → β → γ → δ) → f α → f β → f γ → f δ
𝖋𝖋𝖋 = liftA3
{-# INLINE 𝖋𝖋𝖋 #-}
𝖏𝖋𝖋 ∷ (Monad m) ⇒ (α → β → m a) → m α → m β → m a
𝖏𝖋𝖋 f a b = join $ liftM2 f a b
{-# INLINE 𝖏𝖋𝖋 #-}
𝖕 ∷ (Applicative m) ⇒ α → m α
𝖕 = pure
{-# INLINE 𝖕 #-}
𝖟 ∷ (MonadPlus m) ⇒ m a
𝖟 = mzero
{-# INLINE 𝖟 #-}
mul ∷ (Num a) ⇒ a → a → a
mul = (*)
{-# INLINE mul #-}
