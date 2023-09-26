{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Stanly.Exec(exec) where
import Stanly.Eval(Interpreter(run, step), Store)
import Stanly.Expr(Expr)
import Data.Kind(Type)

-- | Ambiguous type - use with explicit type application: (exec @InterpreterType @ValueType expr)
exec :: forall (m :: Type -> Type) v. Interpreter m v => Expr -> (v, Store v)
exec = run @m @v . step