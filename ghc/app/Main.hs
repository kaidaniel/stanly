{-# LANGUAGE LambdaCase #-}
import Stanly.Concrete (execConcrete, execTrace, execNotCovered)
import Stanly.Abstract (execPowerSet)
import Stanly.Fmt (Fmt (..))
import qualified Stanly.Interpreter as S
import qualified Options.Applicative as O
import qualified Control.Monad as M
import qualified Data.List as L
import Data.Bool (bool)

data Options = Options
  { optValue :: Bool
  , optEcho :: Bool
  , optDesugared :: Bool
  , optAst :: Bool
  , optConcrete :: Bool
  , optTrace :: Bool
  , optNotCovered :: Bool
  , optAbstract :: Bool
  , optNoColour :: Bool
  } deriving (Show)

options :: O.Parser Options
options = Options
      <$> O.switch (O.long "value"       <> O.short 'v' <> O.help "Show the concrete value obtained when the interpreter halts.")
      <*> O.switch (O.long "echo"        <> O.short 'p' <> O.help "Echo the input program.")
      <*> O.switch (O.long "desugared"   <> O.short 'd' <> O.help "Show the program after syntax transformation.")
      <*> O.switch (O.long "ast"         <> O.short 't' <> O.help "Show the abstract syntax tree used by the interpreter.")
      <*> O.switch (O.long "concrete"    <> O.short 'c' <> O.help "Show full detail of the concrete store when the interpreter halts.")
      <*> O.switch (O.long "trace"       <> O.short 't' <> O.help "Show how the interpreter state changes while the program is being evaluated.")
      <*> O.switch (O.long "not-covered" <> O.short 'n' <> O.help "Show subexpressions that weren't reached covered during interpretation.")
      <*> O.switch (O.long "abstract"    <> O.short 'a' <> O.help "Show full detail of the abstract store when the interpreter halts.")
      <*> O.switch (O.long "no-colour"   <> O.short 'w' <> O.help "Don't colourise output.")

main :: IO ()
main = do
  cli_opts <- O.execParser $
          O.info
          (options O.<**> O.helper)
          (
            O.fullDesc
            <> O.progDesc "Discover something interesting about your source code."
            <> O.header "stanly - static analyser"
          )
  str <- getContents
  ast <- either (error . show) return (S.parser "<stdin>" str)
  let c = optNoColour cli_opts
  M.when (optValue cli_opts)      $ do either putStrLn (putVal c) (fst $ execConcrete ast)
  M.when (optEcho cli_opts)       $ do putStrLn str
  M.when (optDesugared cli_opts)  $ do putFmt c ast
  M.when (optAst cli_opts)        $ do print ast
  M.when (optConcrete cli_opts)   $ do putFmt c (snd $ execConcrete ast)
  M.when (optTrace cli_opts)      $ do putFmt c (execTrace ast)
  M.when (optNotCovered cli_opts) $ do putFmt c (execNotCovered ast)
  M.when (optAbstract cli_opts)   $ do putFmt c (execPowerSet ast)
  
  where
    putFmt :: forall a. Fmt a => Bool -> a -> IO ()
    putFmt b = putStrLn . bool termFmt fmt b
    putVal :: Show l => Bool -> S.Val l -> IO()
    putVal b = \case (S.TxtV s) -> putStrLn s; e -> putFmt b e
