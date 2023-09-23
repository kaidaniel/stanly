module Stanly.Eval(eval) where
import Stanly.Expr(Expr(..))
import Stanly.Monads(Interpreter, Value(..), run, ask, local, asks, find, ext, alloc, assign, isZero, delta)

import Data.Function(fix)


eval e = run (fix ev e)

ev :: (Expr -> Interpreter) -> Expr -> Interpreter
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

{-|
>>> eval (Op2 "+" (Num 5) (Num 3))
(8,[])

>>> eval (App (Lam "x" (Lam "y" (Vbl "x"))) (Num 4))
(λy.x ⟦x→ 0⟧,[(0,4)])

>>> eval (App (Lam "x" (Lam "y" (Vbl "x"))) (App (Lam "x" (Lam "y" (Vbl "x"))) (Num 4)))
(λy.x ⟦x→ 1⟧,[(1,λy.x ⟦x→ 0⟧),(0,4)])


|-}
