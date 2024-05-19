{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Stanly.Language (Variable, Op2 (..), Expr (..), subexprs, freeVars) where

import Control.Applicative (Alternative)
import Data.Set qualified as Set
import GHC.Generics (Generic)
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
    Any ∷ Expr
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
    Any → εₐ

freeVars ∷ Expr → Set.Set Variable
freeVars = \case
    Lam x e → freeVars e `Set.difference` (Set.singleton x)
    Num _ → ε₁
    Txt _ → ε₁
    App f x → freeVars f `Set.union` freeVars x
    Op2 _ l r → freeVars l `Set.union` freeVars r
    If' b x y → freeVars b `Set.union` freeVars x `Set.union` freeVars y
    Rec f e → freeVars e `Set.difference` (Set.singleton f)
    Var x → Set.singleton x
    Any → ε₁

instance Arbitrary Expr where
    arbitrary = sized \case
        0 → ω (Num 1)
        n →
            oneof
                [ φ Num (choose (0, 1000))
                , φ Txt char
                , φ Var word
                , ω Any
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
