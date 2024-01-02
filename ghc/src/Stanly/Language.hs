{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Stanly.Language (Variable, Op2 (..), Expr (..), subexprs, freeVars) where

import Control.Applicative (Alternative)
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

type Variable = String

data Expr where
    Var ∷ Variable → Expr
    App ∷ Expr → Expr → Expr
    Lam ∷ Variable → Expr → Expr
    Rec ∷ Variable → Expr → Expr
    Op2 ∷ Op2 → Expr → Expr → Expr
    Num ∷ Integer → Expr
    Txt ∷ String → Expr
    If' ∷ Expr → Expr → Expr → Expr
    deriving (Eq, Ord, Show, Generic)

data Op2 where
    Plus ∷ Op2
    Minus ∷ Op2
    Times ∷ Op2
    Divide ∷ Op2
    deriving (Eq, Ord, Show)

subexprs ∷ (Alternative f) ⇒ Expr → f Expr
subexprs = \case
    Lam _ e → ω e ⫶ subexprs e
    Num _ → εₐ
    Txt _ → εₐ
    App f x → ω f ⫶ ω x ⫶ subexprs f ⫶ subexprs x
    Op2 _ l r → ω l ⫶ ω r ⫶ subexprs l ⫶ subexprs r
    If' b x y → ω b ⫶ ω x ⫶ ω y ⫶ subexprs b ⫶ subexprs x ⫶ subexprs y
    Rec _ e → ω e ⫶ subexprs e
    Var _ → εₐ

freeVars ∷ Expr → [Variable]
freeVars e = [v | Var v ← subexprs e]

instance Fmt Op2 where
    fmt = ("" ⊹) ∘ \case Plus → "+"; Minus → "-"; Times → "*"; Divide → "/"

instance Fmt Expr where
    fmt = \case
        Var x → "" ⊹ x
        App f x → (Dim ⊹ Magenta ⊹ "(") ⊹ paren₁ f ⊹ " " ⊹ x ⊹ (Dim ⊹ Magenta ⊹ ")")
        Lam x fn → (Dim ⊹ "(λ") ⊹ (Bold ⊹ x) ⊹ "." ⊹ paren₂ fn ⊹ (Dim ⊹ ")")
        Rec x fn → (Dim ⊹ "(μ") ⊹ (Bold ⊹ x) ⊹ "." ⊹ paren₂ fn ⊹ (Dim ⊹ ")")
        Op2 o e₁ e₂ → (Dim ⊹ "(") ⊹ e₁ ⊹ " " ⊹ o ⊹ " " ⊹ e₂ ⊹ (Dim ⊹ ")")
        Num n → "" ⊹ n
        Txt s → (Dim ⊹ show s)
        If' tst e₁ e₂ → "(if " ⊹ tst ⊹ " then " ⊹ e₁ ⊹ " else " ⊹ e₂ ⊹ ")"
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
                , φ Var word
                , φ Op2 arbitrary ⊛ half ⊛ half
                , φ App half ⊛ half
                , φ Lam word ⊛ half
                , φ Rec word ⊛ half
                , φ If' half ⊛ half ⊛ half
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
