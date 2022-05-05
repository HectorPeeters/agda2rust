module Agda.Compiler.ToRust where

import Agda.TypeChecking.Monad (Definition, TCM)
import Control.DeepSeq (NFData)
import Control.Monad.Reader (ReaderT (runReaderT))
import Control.Monad.State (StateT, evalStateT)
import GHC.Generics (Generic)

data RustOptions = RustOptions {} deriving (Generic, NFData)

data RustStatement = RustStatement {}

data ToRustState = ToRustState {}

data ToRustEnv = ToRustEnv {toRustOptions :: RustOptions}

type ToRustM a = StateT ToRustState (ReaderT ToRustEnv TCM) a

initToRustEnv :: RustOptions -> ToRustEnv
initToRustEnv opts = ToRustEnv opts

initToRustState :: ToRustState
initToRustState = ToRustState {}

runToRustM :: RustOptions -> ToRustM a -> TCM a
runToRustM opts = (`runReaderT` initToRustEnv opts) . (`evalStateT` initToRustState)

class ToRust a b where
  toRust :: a -> ToRustM b

instance ToRust Definition (Maybe RustStatement) where
  toRust _ = error "Not implemented"
