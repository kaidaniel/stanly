import Control.Monad.Identity (runIdentity)
import Control.Monad.Writer (execWriter)
import Data.List (intersperse)
import Data.Set qualified as Set (map)
import Options.Applicative (
    Parser,
    ParserInfo,
    auto,
    execParser,
    fullDesc,
    header,
    help,
    helper,
    info,
    long,
    metavar,
    option,
    progDesc,
    short,
    showDefault,
    switch,
 )
import Options.Applicative qualified as Opt (value)
import Stanly.Fmt (Fmt (..), FmtStr, bwText, ttyText, (⊹), (⊹\))
import Stanly.Language (Expr)
import Stanly.Mixins (dead, idₘ, trace)
import Stanly.Monads (abstract, concrete)
import Stanly.Parser (parser)
import Stanly.Store (store, value)
import Stanly.Unicode
import Stanly.Val.Value (Val, prune)

data Semantics = Concrete | Abstract deriving (Eq)
instance Show Semantics where show = \case Concrete → "concrete"; Abstract → "abstract"
instance Read Semantics where
    readsPrec _ = \case "concrete" → [(Concrete, "")]; "abstract" → [(Abstract, "")]; _ → []

data Options = Options
    { noValueO
      , storeO
      , pruneO
      , desugaredO
      , astO
      , traceO
      , deadCodeO
      , noColourO
      , sectionHeadersO ∷
        Bool
    , semanticsO ∷ Semantics
    }
    deriving (Read, Show, Eq)

options ∷ Parser Options
options =
    Options
        <$> flag "no-value" 'n' "Don't show calculated value."
            ⊛ flag "store" 's' "Show the final state of the store after the program halts."
            ⊛ flag "prune" 'p' "Prune store before showing (when using --store)."
            ⊛ flag "desugared" 'd' "Show the program after syntax transformation."
            ⊛ flag "ast" 'a' "Show the abstract syntax tree used by the interpreter."
            ⊛ flag
                "trace"
                't'
                "Show how the interpreter state changes while the program is being evaluated."
            ⊛ flag
                "dead-code"
                'e'
                "Show parts of the program that weren't reached during interpretation."
            ⊛ flag "no-colour" 'c' "Don't colourise output."
            ⊛ flag
                "section-headers"
                'i'
                "Introduce each section by a line of '===' and the section's name."
            ⊛ option
                auto
                ( long "semantics"
                    <> short 'm'
                    <> help "Which kind of values to calculate."
                    <> showDefault
                    <> Opt.value Concrete
                    <> metavar "{concrete|abstract}"
                )
  where
    flag l s h = switch (long l ⋄ short s ⋄ help h)

opts ∷ ParserInfo Options
opts =
    info
        (helper ⊛ options)
        ( fullDesc
            ⋄ progDesc "Discover something interesting about your source code."
            ⋄ header "stanly - static analyser"
        )

outputs ∷ Options → Expr → [FmtStr]
outputs Options{..} ast =
    let
        concrete₁ = runIdentity ⎴ concrete idₘ ast
        abstract₁ = runIdentity ⎴ abstract idₘ ast
        -- y = execWriter (concrete dead ast)
        -- x = execWriter (abstract dead ast)
        m ++? (b, title, m₁) = if b then m ++ [if sectionHeadersO then "== " ⊹ title ⊹\ m₁ else m₁] else m
        sections =
            ( case semanticsO of
                Concrete →
                    ε₁
                        ++? (not noValueO, "value", fmt ⎴ value concrete₁)
                        ++? (storeO, "store", fmt ⎴ (if pruneO then φ prune else id) (store concrete₁))
                        ++? (deadCodeO, "dead-code", fmt (execWriter (concrete dead ast)))
                        ++? (traceO, "trace", fmt (execWriter (concrete trace ast)))
                Abstract →
                    ε₁
                        ++? (not noValueO, "value", fmt ⎴ Set.map value abstract₁)
                        ++? (storeO, "store", fmt ⎴ (Set.map store abstract₁))
                        -- ++? (deadCodeO, "dead-code", fmt (execWriter (abstract dead ast)))
                        -- ++? (traceO, "trace", fmt (execWriter (abstract trace ast)))
            )
                ++? (desugaredO, "desugared", fmt ast)
                ++? (astO, "ast", fmt (show ast))
     in
        intersperse (fmt '\n') ⎴ fmap (⊹ '\n') ⎴ sections

main ∷ IO ()
main = do
    Options{..} ← execParser opts
    ast ← getContents ⇉ either (error ∘ show) ω ∘ parser "<stdin>"
    (putStr ∘ if noColourO then bwText else ttyText) (outputs Options{..} ast)
