module Stanly.Val.Internal (
    Val(..),
    prune,
    closure',
    number',
    text',
    Value,
) where

import Control.Monad.Reader (MonadReader (ask))
import Stanly.Env (Env, pruneEnv)
import Stanly.Fmt (Fmt (..), FmtCmd (Bold, Dim), (⊹))
import Stanly.Language (Expr, Variable, freeVars)
import Stanly.Unicode

data Val l where
    LamV ∷ (Fmt l) ⇒ Variable → Expr → Env l → Val l
    NumV ∷ Integer → Val l
    TxtV ∷ String → Val l

class Value val where
    fromVal ∷ Val l → val l
    -- lambda :: (Fmt l, MonadExc m) ⇒ Val l → m (Variable, Expr, Env l)

instance Value Val where
    fromVal = id

deriving instance (Eq l) ⇒ Eq (Val l)

deriving instance (Ord l) ⇒ Ord (Val l)

prune ∷ Val l → Val l
prune = \case
    LamV x e r → LamV x e (pruneEnv (∈ freeVars e) r)
    x → x

number' ∷ (Monad m, Value val) ⇒ Integer → m (val l)
number' = ω ∘ fromVal ∘ NumV

text' ∷ (Monad m, Value val) ⇒ String → m (val l)
text' = ω ∘ fromVal ∘ TxtV

closure' ∷ (Fmt l, MonadReader (Env l) m, Value val) ⇒ Variable → Expr → m (val l)
closure' x e = φ (fromVal ∘ LamV x e) ask

instance Show (Val l) where
    show = \case
        LamV x body _ → "LamV" ⋄ " " ⋄ show x ⋄ " " ⋄ show body
        NumV n → "NumV" ⋄ " " ⋄ show n
        TxtV s → "TxtV" ⋄ " " ⋄ show s

instance (Fmt l) ⇒ Fmt (Val l) where
    fmt = \case
        LamV x body r → "λ" ⊹ (Bold ⊹ x) ⊹ "." ⊹ body ⊹ " " ⊹ r
        NumV n → Dim ⊹ n
        TxtV s → Dim ⊹ s
