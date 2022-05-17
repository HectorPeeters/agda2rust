module Agda.Compiler.ToRust where

import Agda.Compiler.Common
import Agda.Compiler.RustSyntax
import Agda.Compiler.ToTreeless (toTreeless)
import Agda.Compiler.Treeless.EliminateLiteralPatterns
import Agda.Compiler.Treeless.NormalizeNames (normalizeNames)
import Agda.Syntax.Abstract.Name
import Agda.Syntax.Common
import Agda.Syntax.Concrete (Name (nameNameParts))
import Agda.Syntax.Internal as I
import Agda.Syntax.Literal
import Agda.Syntax.Treeless
import Agda.TypeChecking.Monad
import Agda.TypeChecking.Pretty
import Agda.TypeChecking.Primitive.Base
import Agda.Utils.Impossible (__IMPOSSIBLE__)
import Agda.Utils.Lens
import Agda.Utils.List
import Agda.Utils.Maybe
import Agda.Utils.Monad (ifM, unlessM)
import Agda.Utils.Null
import Agda.Utils.Pretty
import qualified Agda.Utils.Pretty as P
import Agda.Utils.Singleton
import Control.DeepSeq (NFData)
import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader
import Control.Monad.State
import Data.Char
import Data.Data (dataTypeName)
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text, replace)
import qualified Data.Text as T
import Debug.Trace (trace)
import GHC.Generics (Generic)
import Prelude hiding (empty, null)

deriving instance Generic EvaluationStrategy

deriving instance NFData EvaluationStrategy

newtype RustOptions = RustOptions
  { rustEvaluation :: EvaluationStrategy
  }
  deriving (Generic, NFData)

data ToRustState = ToRustState
  { toRustFresh :: [Text],
    toRustDefs :: Map QName RsIdent,
    toRustUsedNames :: Set RsIdent
  }

data ToRustEnv = ToRustEnv
  { toRustOptions :: RustOptions,
    toRustVars :: [RsIdent]
  }

type ToRustM a = StateT ToRustState (ReaderT ToRustEnv TCM) a

initToRustEnv :: RustOptions -> ToRustEnv
initToRustEnv opts = ToRustEnv opts []

addBinding :: RsIdent -> ToRustEnv -> ToRustEnv
addBinding x env = env {toRustVars = x : toRustVars env}

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

freshVars :: [Text]
freshVars = concat [map (<> i) xs | i <- "" : map (T.pack . show) [1 ..]]
  where
    xs = map T.singleton ['a' .. 'z']

initToRustState :: ToRustState
initToRustState =
  ToRustState
    { toRustFresh = freshVars,
      toRustDefs = Map.empty,
      toRustUsedNames = reservedNames
    }

runToRustM :: RustOptions -> ToRustM a -> TCM a
runToRustM opts = (`runReaderT` initToRustEnv opts) . (`evalStateT` initToRustState)

getEvaluationStrategy :: ToRustM EvaluationStrategy
getEvaluationStrategy = reader $ rustEvaluation . toRustOptions

class ToRust a b where
  toRust :: a -> ToRustM b

isNameUsed :: RsIdent -> ToRustM Bool
isNameUsed x = gets (Set.member x . toRustUsedNames)

setNameUsed :: RsIdent -> ToRustM ()
setNameUsed x = modify $ \s ->
  s {toRustUsedNames = Set.insert x (toRustUsedNames s)}

rustAllowedUnicodeCats :: Set GeneralCategory
rustAllowedUnicodeCats =
  Set.fromList
    [ UppercaseLetter,
      LowercaseLetter,
      TitlecaseLetter,
      ModifierLetter,
      OtherLetter,
      NonSpacingMark,
      SpacingCombiningMark,
      EnclosingMark,
      DecimalNumber,
      LetterNumber,
      OtherNumber,
      ConnectorPunctuation,
      DashPunctuation,
      OtherPunctuation,
      CurrencySymbol,
      MathSymbol,
      ModifierSymbol,
      OtherSymbol,
      PrivateUse
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
  a <- go $ T.pack $ fixName $ prettyShow $ qnameName n
  return (RsIdent a)
  where
    nextName x = T.pack ('z' : T.unpack x) -- TODO: do something smarter
    go s = ifM (isNameUsed $ RsIdent s) (go $ nextName s) (return s)

    fixName s =
      let s' = concatMap fixChar s
       in if isNumber (head s') then "z" ++ s' else s'

    fixChar c
      | isValidRustChar c = [c]
      | otherwise = "\\x" ++ toHex (ord c) ++ ";"

    toHex 0 = ""
    toHex i = toHex (i `div` 16) ++ [fourBitsToChar (i `mod` 16)]

getDataTypeName :: QName -> Text
getDataTypeName name = T.pack $ prettyShow (nameConcrete (last (mnameToList (qnameModule name))))

getVar :: Int -> ToRustM RsIdent
getVar i = reader $ (!! i) . toRustVars

withFreshVar :: (Text -> ToRustM a) -> ToRustM a
withFreshVar f = do
  strat <- getEvaluationStrategy
  withFreshVar' strat f

withFreshVar' :: EvaluationStrategy -> (Text -> ToRustM a) -> ToRustM a
withFreshVar' strat f = do
  x <- freshRustIdentifier
  local (addBinding $ RsIdent x) $ f x

freshRustIdentifier :: ToRustM Text
freshRustIdentifier = do
  names <- gets toRustFresh
  case names of
    [] -> fail "No more variables!"
    (x : names') -> do
      let ident = RsIdent x
      modify $ \st -> st {toRustFresh = names'}
      ifM (isNameUsed ident) freshRustIdentifier $ {-otherwise-} do
        setNameUsed ident
        return x

lookupRustDef :: QName -> ToRustM (Maybe RsIdent)
lookupRustDef n = gets (Map.lookup n . toRustDefs)

setRustDef :: QName -> RsIdent -> ToRustM ()
setRustDef n a = modify $ \s -> s {toRustDefs = Map.insert n a (toRustDefs s)}

newRustDef :: QName -> ToRustM RsIdent
newRustDef n = do
  unlessM (isNothing <$> lookupRustDef n) __IMPOSSIBLE__
  a <- makeRustName n
  setRustDef n a
  setNameUsed a
  return a

generateFunctionName :: QName -> Text
generateFunctionName = replace "." "_" . T.pack . prettyShow

compileFunction :: Definition -> [RsIdent] -> RsExpr -> Maybe RsItem
compileFunction func argNames body = do
  let def = theDef func
  let name = generateFunctionName $ defName func
  let args = map (\name -> RsArgument name (RsEnumType (RsIdent "Bool"))) argNames
  Just
    ( RsFunction
        (RsIdent name)
        (RsFunctionDecl args (Just (RsEnumType (RsIdent "Bool"))))
        (RsBlock [RsNoSemi body])
    )

instance ToRust Definition (Maybe RsItem) where
  toRust def | defNoCompilation def || not (usableModality $ getModality def) = return Nothing
  toRust def = do
    let f = defName def
    case theDef def of
      Axiom {} -> do
        --        f' <- newRustDef f
        return Nothing
      GeneralizableVar {} -> return Nothing
      Function {} -> do
        strat <- getEvaluationStrategy
        maybeCompiled <- liftTCM $ toTreeless strat f
        case maybeCompiled of
          Just body -> do
            body <- toRust body
            return case body of
              RsClosure args body -> compileFunction def args body
              _ -> __IMPOSSIBLE__
          Nothing -> return Nothing
      Primitive {} -> return Nothing
      PrimitiveSort {} -> return Nothing
      Datatype {dataCons = cons} -> do
        let name = RsIdent (getDataTypeName (head cons))

        variantNames <- mapM makeRustName cons
        variants <- mapM (return . RsVariant) variantNames

        return (Just (RsEnum name variants))
      Record {} -> return Nothing
      Constructor {conSrcCon = chead, conArity = nargs} -> return Nothing
      -- NOTE: Right now the default constructor of the enum is used. This will not work with currying
      -- Constructor {conSrcCon = chead, conArity = nargs} -> do
      --   constructorName <- makeRustName (conName chead)

      --   let dataTypeName = RsIdent (getDataTypeName (conName chead))
      --   let enumName = Just (RsEnumType dataTypeName)
      --   let args = [] -- [RsArgument (RsIdent "x") (RsEnumType (RsIdent "Bool"))]
      --   let body = RsBlock [RsSemi (RsReturn (Just (RsStruct dataTypeName [])))]

      --   -- TODO: add arguments and body
      --   return (Just (RsFunction constructorName (RsFunctionDecl args enumName) body))
      AbstractDefn {} -> __IMPOSSIBLE__
      DataOrRecSig {} -> __IMPOSSIBLE__

instance ToRust TTerm RsExpr where
  toRust v = do
    toRust $ tAppView v

instance ToRust (TTerm, [TTerm]) RsExpr where
  toRust (TCoerce w, args) = toRust (w, args)
  toRust (TApp w args1, args2) = toRust (w, args1 ++ args2)
  toRust (w, args) = do
    -- args <- traverse toRust args
    case w of
      TVar i -> error ("Not implemented " ++ show w)
      TPrim p -> error ("Not implemented " ++ show w)
      TDef d -> error ("Not implemented " ++ show w)
      TLam v -> withFreshVar $ \x -> do
        body <- toRust v
        return (RsClosure [RsIdent x] body)
      TLit l -> error ("Not implemented " ++ show w)
      TCon c -> do
        name <- toRust c
        return (RsDataConstructor name [])
      TLet u v -> error ("Not implemented " ++ show w)
      TCase i info v bs -> do
        cases <- traverse toRust bs
        var <- getVar i
        fallback <-
          if isUnreachable v
            then return Nothing
            else Just <$> toRust v
        return (RsMatch (RsVarRef var) cases fallback)
      TUnit -> error ("Not implemented " ++ show w)
      TSort -> error ("Not implemented " ++ show w)
      TErased -> error ("Not implemented " ++ show w)
      TError err -> error ("Not implemented " ++ show w)

instance ToRust TAlt RsArm where
  toRust (TACon c nargs v) = do
    c' <- toRust c
    result <- toRust v
    return (RsArm (RsDataConstructor c' []) result)
  toRust TAGuard {} = __IMPOSSIBLE__
  toRust TALit {} = __IMPOSSIBLE__

instance ToRust QName RsIdent where
  toRust n = do makeRustName n
