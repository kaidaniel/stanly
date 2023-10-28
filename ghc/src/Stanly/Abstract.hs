module Stanly.Abstract where

import Control.Monad.Identity(Identity)
import Stanly.Interpreter
import Stanly.Fmt
import Stanly.Expr(Var)

data AbstractVal = N

instance Fmt AbstractVal where
    ansiFmt _ = start "N"

type AbstractT m = InterpreterT Int AbstractVal m

instance MonadInterpreter Int AbstractVal (InterpreterT Int AbstractVal Identity) where
    op2 = undefined
    truthy = undefined
    alloc = undefined
    ev = undefined
    destruct = undefined
    construct = undefined