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
import Debug.Trace (trace)
import GHC.Generics (Generic)
import Prelude hiding (empty, null)
import Data.Data (dataTypeName)

data RustOptions = RustOptions {} deriving (Generic, NFData)

data ToRustState = ToRustState
  { toRustDefs :: Map QName RsIdent,
    toRustUsedNames :: Set RsIdent
  }

newtype ToRustEnv = ToRustEnv {toRustOptions :: RustOptions}

type ToRustM a = StateT ToRustState (ReaderT ToRustEnv TCM) a

initToRustEnv :: RustOptions -> ToRustEnv
initToRustEnv = ToRustEnv

reservedNames :: Set RsIdent
reservedNames =
  Set.fromList $
    map
      RsIdent
      [ "if",
        "fn",
        "match",
        "+",
        "-",
        "*",
        "/",
        "true",
        "false"
      ]

initToRustState :: ToRustState
initToRustState = ToRustState {toRustDefs = Map.empty, toRustUsedNames = reservedNames}

runToRustM :: RustOptions -> ToRustM a -> TCM a
runToRustM opts = (`runReaderT` initToRustEnv opts) . (`evalStateT` initToRustState)

class ToRust a b where
  toRust :: a -> ToRustM b

instance ToRust QName RsIdent where
  toRust n = do
    r <- gets (Map.lookup n . toRustDefs)
    case r of
      Nothing -> makeRustName n
      Just a -> return a

saveDefName :: QName -> RsIdent -> ToRustM ()
saveDefName n a = modify $ \s -> s {toRustDefs = Map.insert n a (toRustDefs s)}

isNameUsed :: RsIdent -> ToRustM Bool
isNameUsed x = gets (Set.member x . toRustUsedNames)

getName :: QName -> ToRustM RsIdent
getName x = gets (\y -> (Map.!) (toRustDefs y) x)

setNameUsed :: RsIdent -> ToRustM ()
setNameUsed x = modify $ \s ->
  s {toRustUsedNames = Set.insert x (toRustUsedNames s)}

rustAllowedUnicodeCats :: Set GeneralCategory
rustAllowedUnicodeCats = Set.fromList
  [ UppercaseLetter , LowercaseLetter , TitlecaseLetter , ModifierLetter
  , OtherLetter , NonSpacingMark , SpacingCombiningMark , EnclosingMark
  , DecimalNumber , LetterNumber , OtherNumber , ConnectorPunctuation
  , DashPunctuation , OtherPunctuation , CurrencySymbol , MathSymbol
  , ModifierSymbol , OtherSymbol , PrivateUse
  ]

isValidRustChar :: Char -> Bool
isValidRustChar x
  | isAscii x = isAlphaNum x
  | otherwise = generalCategory x `Set.member` rustAllowedUnicodeCats

fourBitsToChar :: Int -> Char
fourBitsToChar i = "0123456789ABCDEF" !! i
{-# INLINE fourBitsToChar #-}

makeRustName :: QName -> ToRustM RsIdent
makeRustName n = do
  a <- go $ fixName $ prettyShow $ qnameName n
  saveDefName n (RsIdent a)
  setNameUsed (RsIdent a)
  return (RsIdent a)
  where
    nextName = ('z' :) -- TODO: do something smarter  
    go s = ifM (isNameUsed $ RsIdent s) (go $ nextName s) (return s)

    fixName s =
      let s' = concatMap fixChar s
       in if isNumber (head s') then "z" ++ s' else s'

    fixChar c
      | isValidRustChar c = [c]
      | otherwise = "\\x" ++ toHex (ord c) ++ ";"

    toHex 0 = ""
    toHex i = toHex (i `div` 16) ++ [fourBitsToChar (i `mod` 16)]

getDataTypeName :: QName -> String
getDataTypeName name = prettyShow (nameConcrete (last (mnameToList (qnameModule name))))

capitalize :: String -> String
capitalize xs = toUpper (head xs) : tail xs

instance ToRust Definition (Maybe RsItem) where
  toRust def | defNoCompilation def || not (usableModality $ getModality def) = return Nothing
  toRust def = do
    let f = defName def
    case theDef def of
      Axiom {} -> return Nothing
      GeneralizableVar{} -> return Nothing
      Function{} -> return Nothing
      Primitive {} -> return Nothing
      PrimitiveSort {} -> return Nothing
      Datatype {dataCons = cons} -> do
        let name = RsIdent (getDataTypeName (head cons))

        idents <- mapM makeRustName cons
        variants <- mapM (return . RsVariant) idents

        return (Just (RsEnum name variants))
      Record{} -> return Nothing
      Constructor {conSrcCon = chead, conArity = nargs} -> do
        constructorName <- makeRustName (conName chead)

        let dataTypeName = Just (RsEnumType (RsIdent (getDataTypeName (conName chead))))
        let args = []-- [RsArgument (RsIdent "x") (RsEnumType (RsIdent "Bool"))]
        let body = RsBlock []

        -- TODO: add arguments and body
        return (Just (RsFunction constructorName (RsFunctionDecl args dataTypeName) body))
      AbstractDefn{} -> __IMPOSSIBLE__
      DataOrRecSig{} -> __IMPOSSIBLE__

instance ToRust TTerm RsExpr where
  toRust v = do
    v <- liftTCM $ eliminateLiteralPatterns v
    let (w, args) = tAppView v
    -- args' <- traverse toRust args
    case w of
      TVar i -> error "Not implemented"
      TPrim p -> error "Not implemented"
      TDef d -> error "Not implemented"
      TLam v -> error "Not implemented"
      TLit l -> error "Not implemented"
      TCon c -> error "Not implemented"
      TLet u v -> error "Not implemented"
      TCase i info v bs -> error "Not implemented"
      TUnit -> error "Not implemented"
      TSort -> error "Not implemented"
      TErased -> error "Not implemented"
      TCoerce u -> error "Not implemented"
      TError err -> error "Not implemented"
      TApp f args -> __IMPOSSIBLE__
