{-# LANGUAGE ScopedTypeVariables #-}

module Stanly.Concrete (ev, evTrace, evDeadCode, concreteInterpreter) where

import Control.Monad.Except (MonadError, runExceptT, throwError)
import Control.Monad.Identity (runIdentity)
import Control.Monad.Reader (MonadReader (ask, local), runReaderT)
import Control.Monad.State (MonadState, StateT, get, gets, modify, runStateT)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Writer (MonadWriter (tell))
import Data.Char qualified as C
import Data.Coerce (coerce)
import Data.Function (fix)
import Data.Functor.Identity (Identity)
import Data.List qualified as L
import Stanly.Fmt
import Stanly.Interpreter
import Stanly.Interpreter qualified as S
import Stanly.Unicode

type Concrete m = ReaderT (Env Int) (ExceptT String (StateT (Store_ Int) m))

ev ∷ (Show l, Monad m) ⇒ (m (Val l) → Identity c, Interpreter l m) → Expr → c
ev (r, i) = runIdentity . r . fix (S.eval i)

evTrace ∷ ∀ l b m. (Show l, MonadWriter (ProgramTrace l) m) ⇒ (m (Val l) → (ProgramTrace l, b), Interpreter l m) → Expr → ProgramTrace l
evTrace (run, interp) e = fst (run (evalTrace' interp e))
  where
    evalTrace' i expr = do
        r ← env i
        s ← store i
        tell (coerce [(expr, r, s)])
        S.eval i (evalTrace' i) expr

evDeadCode ∷ ∀ l b m. (Show l, MonadWriter (ProgramTrace l) m) ⇒ (m (Val l) → (ProgramTrace l, b), Interpreter l m) → Expr → NotCovered
evDeadCode tpl expr = coerce $ reverse (dead L.\\ (dead >>= S.subexprs))
  where
    dead = S.subexprs expr L.\\ [e | let ProgramTrace li = evTrace tpl expr, (e, _, _) ← li]

runConcrete ∷ Concrete m a → m (Either String a, Store_ Int)
runConcrete m = runStateT (runExceptT (runReaderT m mempty)) mempty

concreteInterpreter ∷ (Monad m) ⇒ (Concrete m a → m (Either String a, Store_ Int), Interpreter Int (Concrete m))
concreteInterpreter = (runConcrete, concreteInterpreter')

concreteInterpreter' ∷ ∀ m. (MonadState (Store_ Int) m, MonadError String m, MonadReader (Env Int) m) ⇒ Interpreter Int m
concreteInterpreter' =
    let exc' er = throwError ("Exception: " ++ er)
     in Interpreter
            { deref = \l → do
                (Store_ store) ← get
                case lookup l store of
                    Just val → 𝖕 val
                    Nothing → error $ show l ++ " not found in store. " ++ fmt (Store_ store)
            , exc = exc'
            , env = ask
            , alloc = \_ → gets length
            , localEnv = local
            , store = get
            , updateStore = modify
            , op2 = \o a b → case (o, a, b) of
                ("+", NumV n0, NumV n1) → (𝖕 ∘ NumV) (n0 + n1)
                ("-", NumV n0, NumV n1) → (𝖕 ∘ NumV) (n0 - n1)
                ("*", NumV n0, NumV n1) → (𝖕 ∘ NumV) (mul n0 n1)
                ("/", NumV n0, NumV n1) →
                    if n1 == 0
                        then exc' ("Division by zero. " ++ show n0 ++ "/" ++ show n1)
                        else 𝖕 $ NumV (div n0 n1)
                ("+", TxtV t0, TxtV t1) → (𝖕 ∘ TxtV) (t0 ++ t1)
                ("+", TxtV t0, NumV n1) → (𝖕 ∘ TxtV) (t0 ++ show n1)
                _ → exc' (invalidOperands o a b)
            , branch = \fls tru → \case
                NumV n → if n /= 0 then tru else fls
                _ → 𝖕 (Undefined "Branching on non-numeric value")
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

newtype ProgramTrace l = ProgramTrace {unProgramTrace ∷ [(Expr, Env l, Store_ l)]} deriving (Eq, Show, Semigroup, Monoid, Foldable)

instance (Show l) ⇒ Fmt (ProgramTrace l) where
    ansiFmt ∷ ProgramTrace l → ANSI
    ansiFmt (ProgramTrace li) = join' (zip (map f li) [1 ..])
      where
        join' ∷ [(ANSI, Integer)] → ANSI
        join' [] = mempty
        join' [(a, i)] = dim >+ show i <> a <> start "\n"
        join' (x : xs) = join' [x] <> join' xs

        f ∷ (Expr, Env l, Store_ l) → ANSI
        f (e, r, s) = dim >+ ("\n" <> name e <> " ") <> ansiFmt e <> dim >+ "\nenvr " <> ansiFmt r <> g s
        g (Store_ []) = start ""
        g x = start "\n" <> ansiFmt x
        name = map C.toLower ∘ L.take 3 ∘ show

newtype NotCovered = NotCovered [Expr] deriving (Eq, Show, Semigroup, Monoid)

instance Fmt NotCovered where
    ansiFmt (NotCovered li) = f li
      where
        f = \case
            [] → mempty
            [x] → ansiFmt x
            (x : xs) → ansiFmt x <> start "\n" <> f xs
