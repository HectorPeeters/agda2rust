module Main where

import Agda.Compiler.Backend
import Agda.Compiler.Common
import Agda.Compiler.ToRust (RustOptions (RustOptions), RustStatement, ToRust (toRust), runToRustM)
import Agda.Interaction.Options (OptDescr)
import Agda.Main (runAgda)
import Agda.Utils.Pretty (prettyShow)
import Control.Monad.Reader (MonadIO (liftIO))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Prelude hiding (empty, null)

main :: IO ()
main = runAgda [backend]

backend :: Backend
backend = Backend backend'

backend' :: Backend' RustOptions RustOptions () () (Maybe RustStatement)
backend' =
  Backend'
    { backendName = "agda2rust",
      options = RustOptions,
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

rustCompileDef :: RustOptions -> () -> IsMain -> Definition -> TCM (Maybe RustStatement)
rustCompileDef opts _ isMain def = runToRustM opts $ toRust def

rustPostModule :: RustOptions -> () -> IsMain -> ModuleName -> [Maybe RustStatement] -> TCM ()
rustPostModule opts _ isMain modName defs = do
  let fileName = prettyShow (last $ mnameToList modName) ++ ".rs"
  liftIO $ T.writeFile fileName ""
