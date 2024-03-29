module Main where

import Agda.Compiler.Backend
import Agda.Compiler.Common
import Agda.Compiler.Hir (HirStmt)
import Agda.Compiler.HirToLir (ToLir (toLir))
import Agda.Compiler.Lir (LirStmt)
import Agda.Compiler.ToRust
  ( RustOptions (RustOptions),
    ToRust (toRust),
    runToRustM,
    rustEvaluation,
    rustNoMain,
  )
import Agda.Interaction.Options (ArgDescr (NoArg), OptDescr (Option))
import Agda.Main (runAgda)
import Agda.Syntax.Treeless (EvaluationStrategy (EagerEvaluation))
import Agda.Utils.Pretty (prettyShow)
import Control.Monad.Reader (MonadIO (liftIO))
import Data.List (intercalate)
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Prelude (unlines)
import Prelude hiding (empty, null)

main :: IO ()
main = runAgda [backend]

backend :: Backend
backend = Backend backend'

backend' :: Backend' RustOptions RustOptions () () [HirStmt]
backend' =
  Backend'
    { backendName = "agda2rust",
      options = RustOptions EagerEvaluation True,
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
rustFlags =
  [ Option
      []
      ["lazy-evaluation"]
      (NoArg $ evaluationFlag LazyEvaluation)
      "Insert delay and force operations to enable lazy evaluation",
    Option
      []
      ["rust-no-main"]
      (NoArg $ noMainFlag True)
      "Don't emit main funtion and module declarations"
  ]

evaluationFlag :: EvaluationStrategy -> Flag RustOptions
evaluationFlag s o = return $ o {rustEvaluation = s}

noMainFlag :: Bool -> Flag RustOptions
noMainFlag s o = return $ o {rustNoMain = s}

rustPreCompile :: RustOptions -> TCM RustOptions
rustPreCompile = return

rustCompileDef :: RustOptions -> () -> IsMain -> Definition -> TCM [HirStmt]
rustCompileDef opts _ isMain def = do
  runToRustM opts $ toRust def

rustPrelude =
  unlines
    [ "#![feature(type_alias_impl_trait)]",
      "#![feature(once_cell)]",
      "#![allow(unconditional_recursion)]",
      "#![allow(non_camel_case_types)]",
      "#![allow(unreachable_patterns)]",
      "#![allow(unused_variables)]",
      "#![allow(dead_code)]",
      "#![allow(non_snake_case)]"
    ]
    ++ "\n"

rustLazyModuleInclude =
  unlines
    [ "mod lazy;",
      "use lazy::Lazy;"
    ]
    ++ "\n"

rustMain = unlines ["", "fn main() {", "println!(\"{:?}\", test());", "}"]

rustPostModule ::
  RustOptions -> () -> IsMain -> ModuleName -> [[HirStmt]] -> TCM ()
rustPostModule opts _ isMain modName defList = do
  let defs :: [HirStmt] = concat defList

  let hirText = intercalate "\n\n" (map show defs)
  let hirFileName = prettyShow (last $ mnameToList modName) ++ ".hir"

  liftIO $ T.writeFile hirFileName (T.pack hirText)

  let evaluationStrategy = rustEvaluation opts
  let noMain = rustNoMain opts

  let lir :: [LirStmt] = toLir defs evaluationStrategy
  let lirText =
        (if noMain then "" else rustPrelude)
          ++ (if evaluationStrategy == LazyEvaluation then rustLazyModuleInclude else "")
          ++ intercalate "\n\n" (map show lir)
          ++ if noMain then "" else rustMain

  let lirFileName = prettyShow (last $ mnameToList modName) ++ ".rs"
  liftIO $ T.writeFile lirFileName (T.pack lirText)
