module Stanly.Fmt where

import Stanly.Expr(Expr(..))
import Stanly.Eval(Env(..), Store(..))
import System.Console.ANSI
import GHC.IO.Handle (BufferMode(BlockBuffering))

colored c x = setSGR [SetColor Foreground Vivid c] >> putStr x >> setSGR [Reset]
vbl = colored Red
addr = colored Blue
num = colored Green

fmtListOfPairs left right li= 
    let
        fmt' sep ((x, a) : r) = sep ++ left x ++ "↦" ++ right a ++ fmt' "," r
        fmt' _ _ = ""
    in 
        fmt' "" li

-- fmtListOfPairsIO left right li =
--     let
--         fmt' ((x, a) : r) = do 
--             print ","
--             l <- left x
--             putStr l
--             print "↦"
--             r <- right a
--             putStr r
--             fmt' r
--         fmt' _ = return ()
--     in 
--         fmt' li

class Fmt a where
  fmt   :: a -> String
  fmtIO :: a -> IO ()
  fmtIO x = putStr (fmt x)

instance Fmt Expr where
  fmt (Vbl x) = x
  fmt (App fn arg) = "(" ++ fmt fn ++ " " ++ fmt arg ++ ")"
  fmt (Lam x body) = "(λ" ++ x ++ "." ++ fmt body ++ ")"
  fmt (Rec f body) = "(μ" ++ f ++ "." ++ fmt body ++ ")"
  fmt (Op2 o left right) = "(" ++ fmt left ++ " " ++ o ++ " " ++ fmt right ++ ")"
  fmt (Num n) = show n
  fmt (If etest etrue efalse) = "(if " ++ fmt etest ++ " then " ++ fmt etrue ++ " else " ++ fmt efalse ++ ")"

instance Fmt Env where
--   fmtIO (Env env') = do { putStr "ℾ⟦"; fmtListOfPairsIO (vbl . show) (addr . show)  env'; putStr "⟧" }
  fmt   (Env env') = "ℾ⟦" ++ fmtListOfPairs id show env' ++ "⟧"


instance Fmt v => Fmt (Store v) where
    fmt :: Store v -> String
    fmt s = "⅀⟦" ++ fmt' s "" ++ "⟧"
        where
            fmt' (Store ((a, v) : r)) sep = sep ++ show a ++ "↦" ++ fmt v ++ fmt' (Store r) ","
            fmt' (Store []) _ = ""
