module Agda.Compiler.ToRust where

import Agda.Compiler.Common
import Agda.Compiler.RustSyntax
import Agda.Compiler.ToTreeless
import Agda.Compiler.Treeless.EliminateLiteralPatterns
import Agda.Syntax.Abstract.Name
import Agda.Syntax.Common
import Agda.Syntax.Concrete (Name (nameNameParts))
import Agda.Syntax.Internal as I
import Agda.Syntax.Literal
import Agda.Syntax.Treeless
import Agda.TypeChecking.Monad
import Agda.TypeChecking.Pretty
import Agda.TypeChecking.Primitive.Base
import Agda.Utils.Impossible
import Agda.Utils.Lens
import Agda.Utils.List
import Agda.Utils.Maybe
import Agda.Utils.Monad
import Agda.Utils.Null
import Agda.Utils.Pretty
import Agda.Utils.Singleton
import Control.DeepSeq (NFData)
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Char
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Prelude hiding (empty, null)

data RustOptions = RustOptions {} deriving (Generic, NFData)

type RustStatement = Item

data ToRustState = ToRustState {}

newtype ToRustEnv = ToRustEnv {toRustOptions :: RustOptions}

type ToRustM a = StateT ToRustState (ReaderT ToRustEnv TCM) a

initToRustEnv :: RustOptions -> ToRustEnv
initToRustEnv = ToRustEnv

initToRustState :: ToRustState
initToRustState = ToRustState {}

runToRustM :: RustOptions -> ToRustM a -> TCM a
runToRustM opts = (`runReaderT` initToRustEnv opts) . (`evalStateT` initToRustState)

class ToRust a b where
  toRust :: a -> ToRustM b

getDataTypeName :: QName -> String
getDataTypeName name = prettyShow (nameConcrete (last (mnameToList (qnameModule name))))

capitalize :: String -> String
capitalize xs = toUpper (head xs) : tail xs

datatypeToRust :: [QName] -> Maybe RustStatement
datatypeToRust cons =
  Just
    ( Enum
        (Ident (getDataTypeName (head cons)))
        (map (Variant . Ident . capitalize . prettyShow . qnameName) cons)
    )

instance ToRust Definition (Maybe RustStatement) where
  toRust def | defNoCompilation def || not (usableModality $ getModality def) = return Nothing
  toRust def = do
    let f = defName def
    case theDef def of
      Axiom {} -> return Nothing
      Datatype {dataCons = cons} -> return (datatypeToRust cons)
      -- Constructor {conSrcCon = chead, conArity = nargs} -> return (constructorToRust chead nargs)
       _ -> return Nothing
