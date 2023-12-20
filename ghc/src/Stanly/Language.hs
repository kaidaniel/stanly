{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Stanly.Language (Var, Op2 (..), Expr (..), subexprs, pruneEnv) where

import Control.Applicative (Alternative)
import Data.Coerce (Coercible, coerce)
import GHC.Generics (Generic)
import Stanly.Fmt (Fmt (..), FmtCmd (Bold, Dim, Magenta), (⊹))
import Stanly.Unicode
import Test.QuickCheck (
    Arbitrary (arbitrary),
    choose,
    elements,
    genericShrink,
    listOf1,
    oneof,
    resize,
    sized,
    suchThat,
 )

type Var = String

data Expr where
    Vbl ∷ Var → Expr
    App ∷ Expr → Expr → Expr
    Lam ∷ Var → Expr → Expr
    Rec ∷ Var → Expr → Expr
    Op2 ∷ Op2 → Expr → Expr → Expr
    Num ∷ Integer → Expr
    Txt ∷ String → Expr
    If ∷ Expr → Expr → Expr → Expr
    deriving (Eq, Show, Generic)

data Op2 where
    Plus ∷ Op2
    Minus ∷ Op2
    Times ∷ Op2
    Divide ∷ Op2
    deriving (Eq, Show)

subexprs ∷ (Alternative f) ⇒ Expr → f Expr
subexprs = \case
    Lam _ e → ω e ⫶ subexprs e
    Num _ → εₐ
    Txt _ → εₐ
    App f x → ω f ⫶ ω x ⫶ subexprs f ⫶ subexprs x
    Op2 _ l r → ω l ⫶ ω r ⫶ subexprs l ⫶ subexprs r
    If b x y → ω b ⫶ ω x ⫶ ω y ⫶ subexprs b ⫶ subexprs x ⫶ subexprs y
    Rec _ e → ω e ⫶ subexprs e
    Vbl _ → εₐ

pruneEnv ∷ ∀ l m n. (Coercible [(Var, l)] (n l), Coercible (m l) [(Var, l)]) ⇒ Expr → m l → n l
pruneEnv e = coerce @[(Var, l)] ∘ filter (flip elem (vbls e) ∘ π₁) ∘ coerce

vbls ∷ Expr → [Var]
vbls e = do Vbl v ← subexprs e; ω v

instance Fmt Op2 where
    fmt = ("" ⊹) ∘ \case Plus → "+"; Minus → "-"; Times → "*"; Divide → "/"

instance Fmt Expr where
    fmt = \case
        Vbl x → "" ⊹ x
        App f x → (Dim ⊹ Magenta ⊹ "(") ⊹ paren₁ f ⊹ " " ⊹ x ⊹ (Dim ⊹ Magenta ⊹ ")")
        Lam x fn → (Dim ⊹ "(λ") ⊹ (Bold ⊹ x) ⊹ "." ⊹ paren₂ fn ⊹ (Dim ⊹ ")")
        Rec x fn → (Dim ⊹ "(μ") ⊹ (Bold ⊹ x) ⊹ "." ⊹ paren₂ fn ⊹ (Dim ⊹ ")")
        Op2 o e₁ e₂ → (Dim ⊹ "(") ⊹ e₁ ⊹ " " ⊹ o ⊹ " " ⊹ e₂ ⊹ (Dim ⊹ ")")
        Num n → "" ⊹ n
        Txt s → (Dim ⊹ show s)
        If tst e₁ e₂ → "(if " ⊹ tst ⊹ " then " ⊹ e₁ ⊹ " else " ⊹ e₂ ⊹ ")"
      where
        paren₁ = \case App f x → paren₁ f ⊹ " " ⊹ x; e → "" ⊹ e
        paren₂ = \case Rec x fn → k "μ" x fn; Lam x fn → k "λ" x fn; e → "" ⊹ e
        k sym x fn = sym ⊹ (Bold ⊹ x) ⊹ "." ⊹ paren₂ fn

instance Arbitrary Expr where
    arbitrary = sized \case
        0 → ω (Num 1)
        n →
            oneof
                [ φ Num (choose (0, 1000))
                , φ Txt char
                , φ Vbl word
                , φ Op2 arbitrary ⊛ half ⊛ half
                , φ App half ⊛ half
                , φ Lam word ⊛ half
                , φ Rec word ⊛ half
                , φ If half ⊛ half ⊛ half
                ]
          where
            half = resize (div n 2) arbitrary
            word = oneof (suchThat char (`notElem` kw) : [ω (x ++ "x") | x ← kw])
            char = listOf1 (elements ['a' .. 'z'])
            kw = ["let", "fn", "rec", "if", "then", "else", "mu", "in"]

instance Arbitrary Op2 where
    arbitrary = elements [Plus, Minus, Times, Divide]

shrink ∷ Expr → [Expr]
shrink = genericShrink
