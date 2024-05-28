module Stanly.Fixed (values, joinedStore, runFixed) where

import Control.Applicative qualified as M
import Control.Monad qualified as M
import Control.Monad.Except qualified as M
import Control.Monad.Reader qualified as M
import Control.Monad.State qualified as M
import Data.List (foldl', nub)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Stanly.Concrete qualified as C
import Stanly.Eval qualified as E
import Stanly.Language qualified as L
import Stanly.Unicode

type Store = Map.Map E.Loc (Set.Set E.Val)
type Cache = E.Cache Store
type Values a = [(a, Store)]
type Exceptions = [(E.Exception, Store)]
type Outputs a = (Cache, Values a, Exceptions)

data Inputs = Inputs
    { env ∷ E.Env
    , store ∷ Store
    , cacheIn ∷ Cache
    , cacheOut ∷ Cache
    }
newtype Fixed a = Fixed {unFixed ∷ Inputs → Outputs a}

runFixed ∷ (Ord a) ⇒ Fixed a → Outputs a
runFixed = \m → unFixed (forceUnique m) Inputs{env = ε₁, store = ε₁, cacheIn = ε₁, cacheOut = mempty}

values ∷ Outputs E.Val → [Either E.Exception E.Val]
values = \(_, vals, exceptions) → nub ([Left e | (e, _) ← exceptions] <> [Right val | (val, _) ← vals])

joinedStore ∷ Outputs a → Store
joinedStore = \(_, vals, exceptions) →
    let store = foldl' (Map.unionWith Set.union) Map.empty ((map π₂ vals) <> (map π₂ exceptions))
        f ∷ Set.Set E.Val → Set.Set E.Val
        f = \set → if E.Any `Set.member` set then Set.singleton E.Any else set
     in Map.map (f ∘ Set.map E.prune) store

forceUnique ∷ (Ord a) ⇒ Fixed a → Fixed a
forceUnique = \(Fixed f) → Fixed \i →
    let (cache, vals, exceptions) = f i
     in (cache, Set.toList ⎴ Set.fromList vals, Set.toList ⎴ Set.fromList exceptions)

instance Functor Fixed where
    fmap = M.liftM

instance Applicative Fixed where
    pure = \x → Fixed \i → (cacheOut i, [(x, store i)], [])
    (<*>) = M.ap

instance Monad Fixed where
    (Fixed m) >>= f = Fixed \inputs →
        let
            (cache, vals, exceptions) = m inputs
            cc (a, s) = (unFixed ⎴ f a) inputs{store = s, cacheOut = cache}
            (caches, vals', exceptions') = unzip3 (map cc vals)
            cache' = foldl' (Map.unionWith Set.union) Map.empty caches
         in
            (cache', M.join vals', M.join exceptions' <> exceptions)

instance M.Alternative Fixed where
    empty = Fixed \i → (cacheOut i, M.empty, M.empty)
    (Fixed f) <|> (Fixed g) = Fixed \inputs →
        let
            (cf, vf, ef) = f inputs
            (cg, vg, eg) = g inputs
         in
            (Map.unionWith Set.union cf cg, vf <> vg, ef <> eg)

instance M.MonadReader E.Env Fixed where
    ask = Fixed \i → (cacheOut i, [(env i, store i)], [])
    local = \f (Fixed m) → Fixed \i → m i{env = f (env i)}

instance M.MonadError E.Exception Fixed where
    throwError = \e → Fixed \i → (cacheOut i, [], [(e, store i)])
    catchError = \(Fixed m) f → Fixed \inputs →
        let
            (cache, vals, exceptions) = m inputs
            cc (a, s) = (unFixed ⎴ f a) inputs{store = s, cacheOut = cache}
            (caches, vals', exceptions') = unzip3 (map cc exceptions)
            cache' = foldl' (Map.unionWith Set.union) Map.empty caches
         in
            (cache', vals <> M.join vals', M.join exceptions')

instance M.MonadState Store Fixed where
    get = Fixed \i → (cacheOut i, [(store i, store i)], [])
    put = \s → Fixed \i → (cacheOut i, [((), s)], [])

truncateV ∷ E.Val → E.Val
truncateV =
    let bound = 1000
     in \case
            E.Num n
                | n > bound → E.Any
                | n < -bound → E.Any
                | otherwise → E.Num n
            E.Txt s
                | length s > (fromIntegral bound) → E.Any
                | otherwise → E.Txt s
            _ → E.Any

instance E.Interpreter Fixed where
    op2 = \cases
        L.Divide lhs E.Any →
            ω E.Any
                ⫶ M.throwError (E.DivisionByZero lhs E.Any)
                ⫶ M.throwError (E.InvalidArgsToOperator lhs L.Divide E.Any)
        o lhs rhs → φ truncateV (C.concreteOp2 o lhs rhs) ⫶ err
          where
            err
                | lhs == E.Any || rhs == E.Any = M.throwError (E.InvalidArgsToOperator lhs o rhs)
                | otherwise = εₐ
    isTruthy = \case
        E.Any → (ω True) ⫶ (ω False) ⫶ M.throwError (E.BranchOnNonNumeric E.Any)
        x → C.concreteIsTruthy x
    find = \loc → Fixed \i →
        let s = store i
         in (cacheOut i, [(v, s) | v ← Set.toList ⎴ fromMaybe ε₁ (s Map.!? loc)], [])
    ext = \loc val → M.modify \s → Map.insertWith (<>) loc (Set.singleton val) s
    alloc var = ω var
