{-# LANGUAGE ScopedTypeVariables #-}

module Stanly.Concrete (ev', evTrace, evDeadCode, concreteInterpreter) where

import Control.Monad.Except (MonadError, runExceptT, throwError)
import Control.Monad.Identity (runIdentity)
import Control.Monad.Reader (MonadReader (ask, local), runReaderT)
import Control.Monad.State (MonadState, StateT, get, gets, modify, runStateT)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Writer (MonadWriter (tell))
import Data.Char qualified as C (toLower)
import Data.Coerce (coerce)
import Data.Function (fix)
import Data.Functor.Identity (Identity)
import Data.List qualified as L (take, (\\))
import Stanly.Fmt
import Stanly.Interpreter qualified as I (Env, Expr, Interpreter (..), Store (..), Val (..), eval, subexprs)
import Stanly.Unicode

type Concrete m = ReaderT (I.Env Int) (ExceptT String (StateT (I.Store Int) m))

ev' ∷ ∀ l m c. (Show l, Monad m) ⇒ (m (I.Val l) → Identity c, I.Interpreter l m) → I.Expr → c
ev' (r, i) = runIdentity . r . fix (I.eval i)

evTrace ∷ ∀ l b m. (Show l, MonadWriter (ProgramTrace l) m) ⇒ (m (I.Val l) → (ProgramTrace l, b), I.Interpreter l m) → I.Expr → ProgramTrace l
evTrace (run, interp) e = fst (run (evalTrace' interp e))
  where
    evalTrace' i expr = do
        r ← I.env i
        s ← I.store i
        tell (coerce [(expr, r, s)])
        I.eval i (evalTrace' i) expr

evDeadCode ∷ ∀ l b m. (Show l, MonadWriter (ProgramTrace l) m) ⇒ (m (I.Val l) → (ProgramTrace l, b), I.Interpreter l m) → I.Expr → NotCovered
evDeadCode tpl expr = coerce (reverse (dead L.\\ (dead >>= I.subexprs)))
  where
    dead = I.subexprs expr L.\\ [e | let ProgramTrace li = evTrace tpl expr, (e, _, _) ← li]

runConcrete ∷ Concrete m a → m (Either String a, I.Store Int)
runConcrete m = runStateT (runExceptT (runReaderT m mempty)) mempty

concreteInterpreter ∷ (Monad m) ⇒ (Concrete m a → m (Either String a, I.Store Int), I.Interpreter Int (Concrete m))
concreteInterpreter = (runConcrete, concreteInterpreter')

concreteInterpreter' ∷ ∀ m. (MonadState (I.Store Int) m, MonadError String m, MonadReader (I.Env Int) m) ⇒ I.Interpreter Int m
concreteInterpreter' =
    let exc' er = throwError ("Exception: " ++ er)
     in I.Interpreter
            { I.deref = \l → do
                (I.Store store) ← get
                case lookup l store of
                    Just val → 𝖕 val
                    Nothing → error $ show l ++ " not found in store. " ++ fmt (I.Store store)
            , I.exc = exc'
            , I.env = ask
            , I.alloc = \_ → gets length
            , I.localEnv = local
            , I.store = get
            , I.updateStore = modify
            , I.op2 = \o a b → case (o, a, b) of
                ("+", I.NumV n0, I.NumV n1) → (𝖕 ∘ I.NumV) (n0 + n1)
                ("-", I.NumV n0, I.NumV n1) → (𝖕 ∘ I.NumV) (n0 - n1)
                ("*", I.NumV n0, I.NumV n1) → (𝖕 ∘ I.NumV) (mul n0 n1)
                ("/", I.NumV n0, I.NumV n1) →
                    if n1 == 0
                        then exc' ("Division by zero. " ++ show n0 ++ "/" ++ show n1)
                        else 𝖕 $ I.NumV (div n0 n1)
                ("+", I.TxtV t0, I.TxtV t1) → (𝖕 ∘ I.TxtV) (t0 ++ t1)
                ("+", I.TxtV t0, I.NumV n1) → (𝖕 ∘ I.TxtV) (t0 ++ show n1)
                _ → exc' (invalidOperands o a b)
            , I.branch = \fls tru → \case
                I.NumV n → if n /= 0 then tru else fls
                _ → 𝖕 (I.Undefined "Branching on non-numeric value")
            }

invalidOperands ∷ (Fmt a1, Fmt a2) ⇒ String → a1 → a2 → String
invalidOperands o a b =
    "Invalid arguments to operator '"
        <> o
        <> "':\n"
        <> "\nleft operand  >>> "
        <> termFmt a
        <> "\noperation     >>> "
        <> o
        <> "\nright operand >>> "
        <> termFmt b

newtype ProgramTrace l = ProgramTrace [(I.Expr, I.Env l, I.Store l)] deriving (Show, Semigroup, Monoid, Foldable)

instance (Show l) ⇒ Fmt (ProgramTrace l) where
    ansiFmt ∷ ProgramTrace l → ANSI
    ansiFmt (ProgramTrace li) = join' (zip (map f li) [1 ..])
      where
        join' ∷ [(ANSI, Integer)] → ANSI
        join' [] = mempty
        join' [(a, i)] = dim >+ show i <> a <> start "\n"
        join' (x : xs) = join' [x] <> join' xs

        f ∷ (I.Expr, I.Env l, I.Store l) → ANSI
        f (e, r, s) = dim >+ ("\n" <> name e <> " ") <> ansiFmt e <> dim >+ "\nenvr " <> ansiFmt r <> g s
        g (I.Store []) = start ""
        g x = start "\n" <> ansiFmt x
        name = map C.toLower ∘ L.take 3 ∘ show

newtype NotCovered = NotCovered [I.Expr] deriving (Eq, Show, Semigroup, Monoid)

instance Fmt NotCovered where
    ansiFmt (NotCovered li) = f li
      where
        f = \case
            [] → mempty
            [x] → ansiFmt x
            (x : xs) → ansiFmt x <> start "\n" <> f xs
