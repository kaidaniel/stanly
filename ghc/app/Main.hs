{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Monad.Identity (runIdentity)
import Control.Monad.Writer (execWriter)
import Data.List (intersperse)
import Options.Applicative (Parser, ParserInfo, execParser, fullDesc, header, help, helper, info, long, progDesc, short, switch)
import Stanly.Fmt (Fmt (..), bwText, ttyText, (⊹), (⊹\))
import Stanly.Mixins (dead, idₘ, trace)
import Stanly.Monads (concrete)
import Stanly.Parser (parser)
import Stanly.Store (store, value)
import Stanly.Unicode
import Stanly.Val (prune)

data Options = Options {valueO, storeO, pruneO, desugaredO, astO, traceO, deadCodeO, noColourO, sectionHeadersO ∷ Bool} deriving (Read, Show, Eq)
isDefault ∷ Options → Bool
isDefault Options{..} = not (storeO || pruneO || desugaredO || astO || traceO || deadCodeO || noColourO || sectionHeadersO)

options ∷ Parser Options
options =
    Options
        <$> flag "value" 'v' "Show calculated value (default if no other options set)."
            ⊛ flag "store" 's' "Show the final state of the store after the program halts."
            ⊛ flag "prune" 'p' "Prune store before showing (when using --store)."
            ⊛ flag "desugared" 'd' "Show the program after syntax transformation."
            ⊛ flag "ast" 'a' "Show the abstract syntax tree used by the interpreter."
            ⊛ flag "trace" 't' "Show how the interpreter state changes while the program is being evaluated."
            ⊛ flag "dead-code" 'e' "Show parts of the program that weren't reached during interpretation."
            ⊛ flag "no-colour" 'n' "Don't colourise output."
            ⊛ flag "section-headers" 'i' "Introduce each section by a line of '===' and the section's name."
  where
    flag l s h = switch (long l ⋄ short s ⋄ help h)

opts ∷ ParserInfo Options
opts =
    info
        (helper ⊛ options)
        (fullDesc ⋄ progDesc "Discover something interesting about your source code." ⋄ header "stanly - static analyser")

main ∷ IO ()
main = do
    Options{..} ← execParser opts
    ast ← either (error ∘ show) ω ∘ parser "<stdin>" =<< getContents
    let concrete₁ = runIdentity ⎴ concrete idₘ ast
    let m ++? (b, title, m₁) = if b then m ++ [if sectionHeadersO then "== " ⊹ title ⊹\ m₁ else m₁] else m
    let sections =
            ε₁
                ++? (valueO || isDefault Options{..}, "value", fmt ⎴ value concrete₁)
                ++? (storeO, "store", fmt ⎴ (if pruneO then φ prune else id) (store concrete₁))
                ++? (desugaredO, "desugared", fmt ast)
                ++? (astO, "ast", fmt (show ast))
                ++? (deadCodeO, "dead-code", fmt (execWriter (concrete dead ast)))
                ++? (traceO, "trace", fmt (execWriter (concrete trace ast)))
    let putFmt = putStr ∘ if noColourO then bwText else ttyText
    putFmt ⎴ intersperse (fmt '\n') (fmap (⊹ '\n') sections)
