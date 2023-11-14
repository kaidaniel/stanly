{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Arrow ((>>>))
import Control.Monad qualified as M
import Data.Bool (bool)
import Options.Applicative qualified as O
import Stanly.Abstract qualified as Abs
import Stanly.Concrete (execConcrete, execNotCovered, execTrace)
import Stanly.Fmt (Fmt (..))
import Stanly.Fmt qualified as F
import Stanly.Interpreter qualified as S

data Options = Options
  { optValue :: String,
    optStore :: String,
    optDesugared :: Bool,
    optAst :: Bool,
    optTrace :: Bool,
    optDeadCode :: Bool,
    optNoColour :: Bool
  }
  deriving (Show)

options :: O.Parser Options
options =
  Options
    <$> O.strOption
      ( O.long "value"
          <> O.short 'v'
          <> O.help "Show the value obtained when the interpreter halts."
          <> O.metavar "{none|concrete|abstract}"
          <> O.showDefault
          <> O.value "concrete"
          <> O.completer (O.listCompleter ["none", "concrete", "abstract"])
      )
    <*> O.strOption
      ( O.long "store"
          <> O.short 's'
          <> O.help "Show the final state of the store after the program halts."
          <> O.metavar "{none|pruned-envs|full-envs}"
          <> O.showDefault
          <> O.value "none"
          <> O.completer (O.listCompleter ["none", "full-envs", "pruned-envs"])
      )
    <*> O.switch (O.long "desugared" <> O.help "Show the program after syntax transformation.")
    <*> O.switch (O.long "ast" <> O.help "Show the abstract syntax tree used by the interpreter.")
    <*> O.switch (O.long "trace" <> O.help "Show how the interpreter state changes while the program is being evaluated.")
    <*> O.switch (O.long "dead-code" <> O.help "Show parts of the program that weren't reached during interpretation.")
    <*> O.switch (O.long "no-colour" <> O.help "Don't colourise output.")

fmtVal :: (Show l) => Fns -> S.Val l -> String
fmtVal Fns {..} = \case (S.TxtV s) -> s; e -> fmt_ e

data Fns = Fns
  { fs :: forall l. (Show l) => S.Store_ l -> String,
    fmt_ :: forall a. (Fmt a) => a -> String
  }

concreteOutput :: Fns -> S.Expr -> String
concreteOutput Fns {..} expr =
  let (v, s) = execConcrete expr
   in either id (fmtVal Fns {..}) v <> "\n" <> fs s

abstractOutput :: Fns -> S.Expr -> String
abstractOutput Fns {..} expr = do
  (v, s) <- Abs.unPowerSet $ Abs.execPowerSet expr
  fmtVal Fns {..} v <> "\n" <> fs s

produceOutput :: Options -> S.Expr -> String
produceOutput Options {..} expr =
  let f = case optValue of
        "concrete" -> concreteOutput
        "abstract" -> abstractOutput
        "none" -> \_ _ -> ""
        _ -> error "Invalid --value option."
      pruneEnv = \case (l, S.LamV x e r) -> (l, S.LamV x e (S.pruneEnv e r)); x -> x
      fmt_ :: forall a. (Fmt a) => a -> String
      fmt_ = if optNoColour then F.fmt else F.termFmt
      fs s = case optStore of
        "none" -> ""
        "full-envs" -> fmt_ s <> "\n"
        "pruned-envs" -> (S.unStore >>> map pruneEnv >>> S.Store_ >>> fmt_ >>> (<> "\n")) s
        _ -> error "Invalid --store option."
   in f Fns {..} expr

main :: IO ()
main = do
  cli_opts <-
    O.execParser $
      O.info
        (options O.<**> O.helper)
        ( O.fullDesc
            <> O.progDesc "Discover something interesting about your source code."
            <> O.header "stanly - static analyser"
        )
  str <- getContents
  ast <- either (error . show) return (S.parser "<stdin>" str)
  putStr (produceOutput cli_opts ast)
  M.when cli_opts.optDesugared $ do putFmt (optNoColour cli_opts) ast
  M.when cli_opts.optAst $ do print ast
  M.when cli_opts.optTrace $ do putFmt (optNoColour cli_opts) (execTrace ast)
  M.when cli_opts.optDeadCode $ do putFmt (optNoColour cli_opts) (execNotCovered ast)
  where
    putFmt :: forall a. (Fmt a) => Bool -> a -> IO ()
    putFmt b = (\s -> if s == "" then putStr "" else putStrLn s) . bool termFmt fmt b
