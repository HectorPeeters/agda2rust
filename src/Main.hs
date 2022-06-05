module Main where

import           Agda.Compiler.Backend
import           Agda.Compiler.Common
import           Agda.Compiler.Hir        (HirStmt)
import           Agda.Compiler.HirToLir   (toLirStmts)
import           Agda.Compiler.RustSyntax (RsItem, rustPrelude)
import           Agda.Compiler.ToRust     (RustOptions (RustOptions),
                                           ToRust (toRust), runToRustM)
import           Agda.Interaction.Options (OptDescr)
import           Agda.Main                (runAgda)
import           Agda.Syntax.Treeless     (EvaluationStrategy (EagerEvaluation))
import           Agda.Utils.Pretty        (prettyShow)
import           Control.Monad.Reader     (MonadIO (liftIO))
import           Data.List                (intercalate)
import           Data.Maybe               (catMaybes)
import qualified Data.Text                as T
import qualified Data.Text.IO             as T
import           Prelude                  hiding (empty, null)

main :: IO ()
main = runAgda [backend]

backend :: Backend
backend = Backend backend'

backend' :: Backend' RustOptions RustOptions () () [HirStmt]
backend' =
  Backend'
    { backendName = "agda2rust"
    , options = RustOptions EagerEvaluation
    , commandLineFlags = rustFlags
    , isEnabled = const True
    , preCompile = rustPreCompile
    , postCompile = \_ _ _ -> return ()
    , preModule = \_ _ _ _ -> return $ Recompile ()
    , compileDef = rustCompileDef
    , postModule = rustPostModule
    , backendVersion = Nothing
    , scopeCheckingSuffices = False
    , mayEraseType = \_ -> return True
    }

rustFlags :: [OptDescr (Flag RustOptions)]
rustFlags = []

rustPreCompile :: RustOptions -> TCM RustOptions
rustPreCompile = return

rustCompileDef :: RustOptions -> () -> IsMain -> Definition -> TCM [HirStmt]
rustCompileDef opts _ isMain def = do
  hir <- runToRustM opts $ toRust def
  return hir

rustPostModule ::
     RustOptions -> () -> IsMain -> ModuleName -> [[HirStmt]] -> TCM ()
rustPostModule opts _ isMain modName defList = do
  let defs :: [HirStmt] = concat defList
  let hirText = intercalate "\n\n" (map show defs)
  let hirFileName = prettyShow (last $ mnameToList modName) ++ ".hir"
  liftIO $ T.writeFile hirFileName (T.pack hirText)
  let lir = toLirStmts defs
  let lirText = intercalate "\n\n" (map show lir)
  let lirFileName = prettyShow (last $ mnameToList modName) ++ ".rs"
  liftIO $ T.writeFile lirFileName (T.pack lirText)
