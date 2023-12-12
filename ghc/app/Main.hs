{-# LANGUAGE RecordWildCards #-}

import Control.Arrow ((>>>))
import Control.Monad qualified as M
import Data.Coerce (coerce)
import Data.List qualified as L
import Options.Applicative qualified as O
import Stanly.Abstract qualified as Abs
import Stanly.Concrete
import Stanly.Fmt qualified as F
import Stanly.Interpreter qualified as S

options ∷ O.Parser Options
options =
    Options
        <$> choice
            "value"
            [Concrete, NoneV, Abstract]
            "Show the value obtained when the interpreter halts."
        <*> choice "store" [NoneS, Full, Pruned] "Show the final state of the store after the program halts."
        <*> flag "desugared" "Show the program after syntax transformation."
        <*> flag "ast" "Show the abstract syntax tree used by the interpreter."
        <*> flag "trace" "Show how the interpreter state changes while the program is being evaluated."
        <*> flag "dead-code" "Show parts of the program that weren't reached during interpretation."
        <*> flag "no-colour" "Don't colourise output."
  where
    choice long li help =
        O.option O.auto $
            O.long long
                <> O.short (head long)
                <> O.showDefault
                <> O.value (head li)
                <> O.metavar ("{" <> L.intercalate "|" (map show li) <> "}")
                <> O.help help
    flag long help = O.switch (O.long long <> O.help help)

data Fns = Fns
    { show_store ∷ ∀ l. (Show l) ⇒ S.Store l → String
    , fmt' ∷ ∀ a. (F.Fmt a) ⇒ a → String
    }

main ∷ IO ()
main = do
    o@Options{} ← opts
    ast ← either (error . show) pure . S.parser "<stdin>" =<< getContents
    value o ast
    flags o ast
  where
    flags ∷ Options → S.Expr → IO ()
    flags Options{desugaredO, astO, deadCodeO, noColourO, traceO} ast = do
        M.when desugaredO (putFmt ast)
        M.when astO (print ast)
        M.when deadCodeO (putFmt (evDeadCode concreteInterpreter ast))
        M.when traceO (putFmt (evTrace concreteInterpreter ast))
      where
        fmt_ ∷ ∀ a. (F.Fmt a) ⇒ a → String
        fmt_ = if noColourO then F.fmt else F.termFmt
        putFmt x = if fmt_ x == "" then putStr "" else putStrLn $ fmt_ x
    value Options{valueO, noColourO, storeO} =
        putStr . case valueO of
            Concrete → concreteOutput Fns{..}
            Abstract → abstractOutput Fns{..}
            NoneV → const ""
      where
        pruneEnv = \case (l, S.LamV x e r) → (l, S.LamV x e (S.pruneEnv e r)); x → x
        fmt' ∷ ∀ a. (F.Fmt a) ⇒ a → String
        fmt' = if noColourO then F.fmt else F.termFmt
        show_store ∷ ∀ l. (Show l) ⇒ S.Store l → String
        show_store s = case storeO of
            NoneS → ""
            Full → fmt' s <> "\n"
            Pruned → (coerce @_ @[(l, S.Val l)] >>> map pruneEnv >>> S.Store >>> fmt' >>> (<> "\n")) s
    fmtVal Fns{..} = \case (S.TxtV s) → s; e → fmt' e
    abstractOutput Fns{..} expr = do (v, s) ← Abs.unPowerSet $ Abs.execPowerSet expr; fmtVal Fns{..} v <> "\n" <> show_store s
    concreteOutput Fns{..} expr = do (v, s) ← [ev concreteInterpreter expr]; either id (fmtVal Fns{..}) v <> "\n" <> show_store s
    opts = O.execParser $ O.info (O.helper <*> options) desc
      where
        desc = O.fullDesc <> progDesc <> header
        progDesc = O.progDesc "Discover something interesting about your source code."
        header = O.header "stanly - static analyser"

data Options = Options
    {valueO ∷ ValueO, storeO ∷ StoreO, desugaredO, astO, traceO, deadCodeO, noColourO ∷ Bool}
    deriving (Read, Show, Eq)

data ValueO = NoneV | Concrete | Abstract deriving (Eq)

data StoreO = NoneS | Full | Pruned deriving (Eq)

instance Show ValueO where
    show = \case NoneV → "none"; Concrete → "concrete"; Abstract → "abstract"

instance Show StoreO where
    show = \case NoneS → "none"; Full → "full"; Pruned → "pruned"

instance Read ValueO where
    readsPrec _ = \case "none" → [(NoneV, "")]; "concrete" → [(Concrete, "")]; "abstract" → [(Abstract, "")]; _ → []

instance Read StoreO where
    readsPrec _ = \case "none" → [(NoneS, "")]; "full" → [(Full, "")]; "pruned" → [(Pruned, "")]; _ → []
