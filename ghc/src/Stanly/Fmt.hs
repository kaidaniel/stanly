module Stanly.Fmt (FmtStr, FmtCmd (..), (|-|), (⊹), Fmt (fmt), bwText, ttyText, (⊹\)) where

-- no-haskell-unicode

import Data.Char (isSpace)
import Data.List (intercalate)
import Data.Set qualified as Set (Set, toList)
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
instance (Fmt a) ⇒ Fmt (Set.Set a) where fmt = fmt . (map fmt) . Set.toList
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
