module Stanly.Abstract (values, joinedStore, run) where

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
newtype Abstract a = Abstract {unAbstract ∷ Inputs → Outputs a}

run ∷ (Ord a) ⇒ Abstract a → Outputs a
run = \m →
    unAbstract (forceUnique m) Inputs{env = ε₁, store = ε₁, cacheIn = ε₁, cacheOut = mempty}

values ∷ Outputs E.Val → [Either E.Exception E.Val]
values = \(_, vals, exceptions) → nub ([Left e | (e, _) ← exceptions] <> [Right val | (val, _) ← vals])

squashToTop ∷ Set.Set E.Val → Set.Set E.Val
squashToTop = \set → if E.Any `Set.member` set then Set.singleton E.Any else set

joinedStore ∷ Outputs a → Store
joinedStore = \(_, vals, exceptions) →
    let s = foldl' (Map.unionWith Set.union) Map.empty ((map π₂ vals) <> (map π₂ exceptions))
     in Map.map (squashToTop ∘ Set.map E.prune) s

forceUnique ∷ (Ord a) ⇒ Abstract a → Abstract a
forceUnique = \(Abstract f) → Abstract \i →
    let (cache, vals, exceptions) = f i
     in (cache, Set.toList ⎴ Set.fromList vals, Set.toList ⎴ Set.fromList exceptions)

instance Functor Abstract where
    fmap = M.liftM

instance Applicative Abstract where
    pure = \x → Abstract \i → (cacheOut i, [(x, store i)], [])
    (<*>) = M.ap

instance Monad Abstract where
    (Abstract m) >>= f = Abstract \inputs →
        let
            (cache, vals, exceptions) = m inputs
            cc (a, s) = (unAbstract ⎴ f a) inputs{store = (Map.map squashToTop s), cacheOut = cache}
            (caches, vals', exceptions') = unzip3 (map cc vals)
            cache' = foldl' (Map.unionWith Set.union) Map.empty caches
         in
            (cache', M.join vals', M.join exceptions' <> exceptions)

instance M.Alternative Abstract where
    empty = Abstract \i → (cacheOut i, M.empty, M.empty)
    (Abstract f) <|> (Abstract g) = Abstract \inputs →
        let
            (cf, vf, ef) = f inputs
            (cg, vg, eg) = g inputs
         in
            (Map.unionWith Set.union cf cg, vf <> vg, ef <> eg)

instance M.MonadReader E.Env Abstract where
    ask = Abstract \i → (cacheOut i, [(env i, store i)], [])
    local = \f (Abstract m) → Abstract \i → m i{env = f (env i)}

instance M.MonadError E.Exception Abstract where
    throwError = \e → Abstract \i → (cacheOut i, [], [(e, store i)])
    catchError = \(Abstract m) f → Abstract \inputs →
        let
            (cache, vals, exceptions) = m inputs
            cc (a, s) = (unAbstract ⎴ f a) inputs{store = s, cacheOut = cache}
            (caches, vals', exceptions') = unzip3 (map cc exceptions)
            cache' = foldl' (Map.unionWith Set.union) Map.empty caches
         in
            (cache', vals <> M.join vals', M.join exceptions')

instance M.MonadState Store Abstract where
    get = Abstract \i → (cacheOut i, [(store i, store i)], [])
    put = \s → Abstract \i → (cacheOut i, [((), s)], [])

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

instance E.Interpreter Abstract where
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
    find = \loc → Abstract \i →
        let s = store i
         in (cacheOut i, [(v, s) | v ← Set.toList ⎴ fromMaybe ε₁ (s Map.!? loc)], [])
    ext = \loc val → M.modify \s → Map.insertWith (<>) loc (Set.singleton val) s
    alloc var = ω var
