{-# LANGUAGE ScopedTypeVariables #-}

module Stanly.Concrete (runConcrete, evalConcrete) where

import Control.Monad.Except
import Control.Monad.Reader (
    MonadReader (ask, local),
    ReaderT (runReaderT),
 )
import Control.Monad.State
import Stanly.Fmt
import Stanly.Interpreter qualified as I
import Stanly.Unicode

type EnvT m = ReaderT (I.Env Int) m
type StoreT m = StateT (I.Store Int) m
type ExcT m = ExceptT String m

type ConcreteT m = ExcT (EnvT (StoreT m))

evalConcrete ∷ ∀ m. (Monad m) ⇒ I.Combinator Int (ConcreteT m) → I.Expr → m (Either String (I.Val Int))
evalConcrete c e = do (v, _) ← runConcrete c e; pure v

runConcrete ∷ ∀ m. (Monad m) ⇒ I.Combinator Int (ConcreteT m) → I.Expr → m (Either String (I.Val Int), I.Store Int)
runConcrete ev = rstore . renv . rexc . ev'
  where
    rstore = flip runStateT mempty
    renv = flip runReaderT mempty
    rexc = runExceptT
    ev' = ev concreteInterpreter
    concreteInterpreter =
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
