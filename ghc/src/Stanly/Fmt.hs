{-# LANGUAGE BlockArguments #-}

module Stanly.Fmt (FmtCmd (..), FmtStr (..), (|-|), (⊹), Fmt (..), bwText, ttyText) where

-- no-haskell-unicode

import Data.List qualified as L
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

instance Semigroup FmtStr where
    Empty <> a = a
    a <> Empty = a
    Str NoFmtCmd a <> Str cmd b = Str cmd (a <> b)
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
        cmds → "\x1b[" <> L.intercalate ";" (show <$> cmds) <> "m"
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
instance Fmt FmtCmd where fmt cmd = Str cmd ""

(⊹), (|-|) ∷ (Fmt a, Fmt b) ⇒ a → b → FmtStr
(|-|) a b = fmt a ⋄ fmt b
(⊹) = (|-|)
infixr 6 |-|, ⊹
{-# INLINE (⊹) #-}
