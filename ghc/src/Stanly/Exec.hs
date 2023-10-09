{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Stanly.Exec(exec) where
import Stanly.Eval(Interpreter(run, ev), Store)
import Stanly.Expr(Expr)
import Data.Kind(Type)

-- | Ambiguous type - use with explicit type application: (exec @InterpreterType @ValueType expr)
exec :: forall (m :: Type -> Type) a v. Interpreter m v a => Expr -> (Either String v, Store a v)
exec = run @m @v . ev