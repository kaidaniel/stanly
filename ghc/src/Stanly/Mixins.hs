module Stanly.Mixins where

import Control.Monad.State
import Data.Map
import Stanly.Unicode

-- following https://www.cs.ox.ac.uk/files/2136/Mixins.pdf

type Mixin s = s → s → s

(.>) ∷ Mixin s → Mixin s → Mixin s
f .> g = \cc call → f (g cc call) call

{-# INLINEABLE mix #-}
mix ∷ Mixin s → s
mix f = let m = mix f in f m m

fib ∷ (Monad m) ⇒ Mixin (Int → m Int)
fib _ call = \n → case n of
    0 → pure 0
    1 → pure 1
    _ → do
        x ← call (n - 1)
        y ← call (n - 2)
        pure (x + y)

memo ∷ (Ord a, MonadState (Map a b) m) ⇒ Mixin (a → m b)
memo cc _ = \x → do
    m ← get
    case m !? x of
        Just x' → pure x'
        Nothing → do
            fx ← cc x
            m' ← get
            put (insert x fx m')
            pure fx

fib' ∷ Int → Int
fib' n = evalState (mix (memo .> fib) n) empty
