module Stanly.Fmt (FmtStr, FmtCmd (..), (|-|), (⊹), Fmt (fmt), bwText, ttyText, (⊹\)) where

-- no-haskell-unicode

import Data.Bifunctor (Bifunctor (bimap))
import Data.Char (isSpace, toLower)
import Data.Coerce
import Data.List (intercalate, intersperse)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Stanly.Concrete (Store (..))
import Stanly.Eval as E (Env (..), Exception, Loc, Trace (..), Val (..))
import Stanly.Language as L (Expr (..), Op2 (..))

import Stanly.Unicode

data FmtCmd where
    Reset ∷ FmtCmd
    Default ∷ FmtCmd
    NoFmtCmd ∷ FmtCmd
    Bold ∷ FmtCmd
    Dim ∷ FmtCmd
    Italic ∷ FmtCmd
    Underline ∷ FmtCmd
    Black ∷ FmtCmd
    Red ∷ FmtCmd
    Green ∷ FmtCmd
    Yellow ∷ FmtCmd
    Blue ∷ FmtCmd
    Magenta ∷ FmtCmd
    Cyan ∷ FmtCmd
    White ∷ FmtCmd
    Compound ∷ FmtCmd → FmtCmd → FmtCmd
    deriving (Eq, Show)

instance Semigroup FmtCmd where
    a <> b | a == b = a
    a <> c@(Compound b _) | a == b = c
    c@(Compound _ a) <> b | a == b = c
    _ <> Reset = Reset
    _ <> Default = Default
    NoFmtCmd <> a = a
    a <> NoFmtCmd = a
    a <> b = Compound a b

instance Monoid FmtCmd where
    mempty = NoFmtCmd

data FmtStr where
    Str ∷ FmtCmd → String → FmtStr
    Node ∷ FmtStr → FmtStr → FmtStr
    Empty ∷ FmtStr
    deriving (Eq, Show)

instance Ord FmtStr where compare l r = compare (ttyText l) (ttyText r)

isSpace' ∷ FmtStr → Bool
isSpace' = \case
    Str _ s → all isSpace s
    Node a b → isSpace' a && isSpace' b
    Empty → True

instance Semigroup FmtStr where
    Empty <> a = a
    a <> Empty = a
    Str cmd a <> Str NoFmtCmd b = Str cmd (a <> b)
    Str cmda "" <> Str cmdb "" = Str (cmda <> cmdb) ""
    a <> b = Node a b

instance Monoid FmtStr where
    mempty = Empty

bwText, ttyText ∷ (Fmt a) ⇒ a → String
bwText = bwText' ∘ fmt
ttyText = ttyText' ∘ fmt

bwText', ttyText' ∷ FmtStr → String
bwText' (Str _ s) = s
bwText' (Node a b) = bwText' a <> bwText' b
bwText' Empty = ""
ttyText' = \case
    Str cmd s → ansi cmd <> s <> ansi Reset
    Node a b → ttyText' a <> ttyText' b
    Empty → ""
  where
    ansi cmd = case codes cmd of
        [] → ""
        cmds → "\x1b[" <> intercalate ";" (show <$> cmds) <> "m"
    codes ∷ FmtCmd → [Integer]
    codes = \case
        Reset → [0]
        Default → [39]
        Bold → [1]
        Dim → [2]
        Italic → [3]
        Underline → [4]
        Black → [90]
        Red → [31]
        Green → [32]
        Yellow → [33]
        Blue → [34]
        Magenta → [35]
        Cyan → [36]
        White → [37]
        NoFmtCmd → []
        Compound a b → codes a <> codes b

display ∷ String → FmtStr
display = Str NoFmtCmd

class Fmt a where fmt ∷ a → FmtStr
instance Fmt Integer where fmt = display ∘ show
instance Fmt Int where fmt = display ∘ show
instance Fmt FmtStr where fmt = id
instance Fmt Char where fmt = display ∘ ω
instance Fmt String where fmt = display
instance Fmt [FmtStr] where fmt = κ₁
instance (Fmt a) ⇒ Fmt (Set.Set a) where
    fmt s = fmt (intersperse (fmt "\n") [fmt x | x ← Set.toList s])

instance Fmt FmtCmd where fmt cmd = Str cmd ""
instance (Fmt a, Fmt b) ⇒ Fmt (Either a b) where
    fmt = \case Left x → fmt x; Right x → fmt x

(|-|), (⊹), (⊹\) ∷ (Fmt a, Fmt b) ⇒ a → b → FmtStr
a |-| b = fmt a ⋄ fmt b
(⊹) = (|-|)
a ⊹\ b =
    let
        a1' = fmt a
        b1' = fmt b
     in
        case (isSpace' a1', isSpace' b1') of
            (True, True) → fmt ""
            (True, _) → b1'
            (_, True) → a1'
            _ → a1' ⊹ '\n' ⊹ b1'

infixr 6 |-|, ⊹, ⊹\

{-# INLINE (⊹) #-}

instance Fmt Val where
    fmt = \case
        E.Lam x body r → "λ" ⊹ (Bold ⊹ x) ⊹ "." ⊹ body ⊹ " " ⊹ r
        E.Num n → Dim ⊹ n
        E.Txt s → Dim ⊹ s
        E.Any → Dim ⊹ (show E.Any)

instance Fmt Env where
    fmt r = (Yellow ⊹ "Γ⟦") ⊹ fmt2 r "" ⊹ (Yellow ⊹ "⟧")
      where
        fmt₁ ∷ [(String, String)] → String → FmtStr
        fmt₁ = \cases
            ((v, a) : r₁) sep → sep ⊹ v ⊹ ": " ⊹ (Yellow ⊹ a) ⊹ fmt₁ r₁ ", "
            [] _ → ε₁
        fmt2 ∷ Env → String → FmtStr
        fmt2 = coerce fmt₁

instance Fmt Exception where
    fmt e = fmt (show e)

instance Fmt (Map.Map E.Loc (Set.Set E.Val)) where
    fmt σ = κ₁ ⎴ intersperse (fmt '\n') lines_
      where
        lines_ = φ (\(loc, val) → (Yellow ⊹ (bwText $ pad loc)) ⊹ " ↦ " ⊹ fval val) items
        items = Map.toAscList σ
        loc_length = minimum [20, (maximum (φ (length . fst) items))]
        fval val = case Set.toList val of
            [E.Any] → Reset ⊹ "⊤"
            [x] → fmt x
            [] → fmt ""
            xs → "{" ⊹ (intersperse (fmt ", ") (map fmt $ xs)) ⊹ "}"
        pad loc = (concat $ replicate (loc_length - length loc) " ") ++ loc

instance Fmt Stanly.Concrete.Store where
    fmt (Stanly.Concrete.MkStore σ) = κ₁ ⎴ intersperse (fmt '\n') items
      where
        f₁ loc = (Dim ⊹ "store ") ⊹ (Yellow ⊹ (padded ⎴ bwText loc))
        f₂ val = Dim ⊹ [toLower c | c ← take 3 (show val)] ⊹ " " ⊹ val
        items = φ (\(l, r) → l ⊹ ' ' ⊹ r) ⎴ φ (bimap f₁ f₂) (Map.toAscList σ)
        padded = \case
            [] → "    "
            s@[_] → "   " ⋄ s
            s@[_, _] → "  " ⋄ s
            s@[_, _, _] → " " ⋄ s
            s → s
instance Fmt Op2 where
    fmt = ("" ⊹) ∘ \case Plus → "+"; Minus → "-"; Times → "*"; Divide → "/"

instance Fmt Expr where
    fmt = \case
        L.Var x → "" ⊹ x
        L.App f x → (Dim ⊹ Magenta ⊹ "(") ⊹ paren₁ f ⊹ " " ⊹ x ⊹ (Dim ⊹ Magenta ⊹ ")")
        L.Lam x fn → (Dim ⊹ "(λ") ⊹ (Bold ⊹ x) ⊹ "." ⊹ paren₂ fn ⊹ (Dim ⊹ ")")
        L.Rec x fn → (Dim ⊹ "(μ") ⊹ (Bold ⊹ x) ⊹ "." ⊹ paren₂ fn ⊹ (Dim ⊹ ")")
        L.Op2 o e₁ e₂ → (Dim ⊹ "(") ⊹ e₁ ⊹ " " ⊹ o ⊹ " " ⊹ e₂ ⊹ (Dim ⊹ ")")
        L.Num n → "" ⊹ n
        L.Txt s → (Dim ⊹ show s)
        L.If' tst e₁ e₂ → "(if " ⊹ tst ⊹ " then " ⊹ e₁ ⊹ " else " ⊹ e₂ ⊹ ")"
        L.Any → Magenta ⊹ "?"
      where
        paren₁ = \case L.App f x → paren₁ f ⊹ " " ⊹ x; e → "" ⊹ e
        paren₂ = \case L.Rec x fn → k "μ" x fn; L.Lam x fn → k "λ" x fn; e → "" ⊹ e
        k sym x fn = sym ⊹ (Bold ⊹ x) ⊹ "." ⊹ paren₂ fn

instance (Fmt a) ⇒ Fmt (Trace a) where
    fmt (Trace li) = κ₁ (φ ln (zip li [1 ∷ Integer ..]))
      where
        low3 e = [toLower x | x ← take 3 ⎴ show e] <> "  "
        ln ((e, ρ, σ), i) = (Dim ⊹ i ⊹\ low3 e) ⊹ " " ⊹ (e ⊹\ ((Dim ⊹ "envir ") ⊹ ρ) ⊹\ σ) ⊹ "\n"
