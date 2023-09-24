module Stanly.Eval (eval) where

import Data.Function (fix)
import Stanly.Expr (Expr (..), Value (..), assign)

eval run ask local asks find ext alloc assign isZero delta = eval'
  where
    eval' e = run (fix ev e)
    ev ev' e = case e of
      Num n -> return $ NumV n
      Vbl x -> do
        r <- ask
        find r x
      If etest etrue efalse -> do
        NumV n <- ev' etest
        z <- isZero n
        ev' $ if z then etrue else efalse
      Op2 o left right -> do
        NumV l <- ev' left
        NumV r <- ev' right
        delta o l r
      Rec f body -> do
        r <- ask
        a <- alloc f
        v <- local (const $ assign f a r) (ev' body)
        ext a v
        return v
      Lam x body -> asks (LamV x body)
      App fn arg -> do
        LamV x body r <- ev' fn
        v <- ev' arg
        a <- alloc x
        ext a v
        local (const $ assign x a r) (ev' body)
