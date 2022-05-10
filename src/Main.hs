module Main where

import Agda.Compiler.Backend
import Agda.Compiler.Common
import Agda.Compiler.ToRust (RustOptions (RustOptions), ToRust (toRust), runToRustM)
import Agda.Compiler.RustSyntax (RsItem)
import Agda.Interaction.Options (OptDescr)
import Agda.Main (runAgda)
import Agda.Utils.Pretty (prettyShow)
import Control.Monad.Reader (MonadIO (liftIO))
import Data.List (intercalate)
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Prelude hiding (empty, null)
import Agda.Syntax.Treeless (EvaluationStrategy(EagerEvaluation))

main :: IO ()
main = runAgda [backend]

backend :: Backend
backend = Backend backend'

backend' :: Backend' RustOptions RustOptions () () (Maybe RsItem)
backend' =
  Backend'
    { backendName = "agda2rust",
      options = RustOptions EagerEvaluation,
      commandLineFlags = rustFlags,
      isEnabled = const True,
      preCompile = rustPreCompile,
      postCompile = \_ _ _ -> return (),
      preModule = \_ _ _ _ -> return $ Recompile (),
      compileDef = rustCompileDef,
      postModule = rustPostModule,
      backendVersion = Nothing,
      scopeCheckingSuffices = False,
      mayEraseType = \_ -> return True
    }

rustFlags :: [OptDescr (Flag RustOptions)]
rustFlags = []

rustPreCompile :: RustOptions -> TCM RustOptions
rustPreCompile = return

rustCompileDef :: RustOptions -> () -> IsMain -> Definition -> TCM (Maybe RsItem)
rustCompileDef opts _ isMain def = runToRustM opts $ toRust def

rustPostModule :: RustOptions -> () -> IsMain -> ModuleName -> [Maybe RsItem] -> TCM ()
rustPostModule opts _ isMain modName defs = do
  let modText = intercalate "\n\n" $ map show (catMaybes defs)
      fileName = prettyShow (last $ mnameToList modName) ++ ".rs"
  liftIO $ T.writeFile fileName (T.pack modText)
