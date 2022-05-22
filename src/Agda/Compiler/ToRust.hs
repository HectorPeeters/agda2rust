module Agda.Compiler.ToRust where

import Agda.Compiler.Backend (TTerm)
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
    toRustUsedNames :: Set RsIdent,
    toRustEnums :: [RsVariant]
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

getVarName :: Int -> ToRustM RsIdent
getVarName i = reader $ (!! i) . toRustVars

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
      toRustUsedNames = reservedNames,
      toRustEnums = []
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

addRustEnumVariant :: RsVariant -> ToRustM ()
addRustEnumVariant variant = modify $ \s -> s {toRustEnums = variant : toRustEnums s}

--clearAndGetEnumVariants :: ToRustM [RsVariant]
--clearAndGetEnumVariants = do
--  variants <- gets toRustEnums
--  modify $ \s -> s {toRustEnums = []}
--  return variants

generateFunctionName :: QName -> Text
generateFunctionName = replace "." "_" . T.pack . prettyShow

removeLastItem :: [a] -> [a]
removeLastItem [] = []
removeLastItem [x] = []
removeLastItem (x : xs) = x : removeLastItem xs

extractTypes :: Term -> [RsIdent]
extractTypes x = case x of
  Var n _ -> [RsIdent (T.pack (['A' ..] !! n : []))]
  Def name _ -> [RsIdent $ T.pack $ prettyShow $ qnameName name]
  Pi dom abs -> do
    let first = extractTypes $ unEl $ unDom dom
    let rest = extractTypes $ unEl $ unAbs abs
    first ++ rest
  _ -> trace ("NOT IMPLEMENTED " ++ show x) []

compileFunction :: Definition -> TTerm -> RsExpr -> [RsItem]
compileFunction func tl body = do
  let def = theDef func
  let name = generateFunctionName $ defName func
  let args = extractTypes $ unEl $ defType func
  let arguments = map RsEnumType (removeLastItem args)
  let return = Just $ RsEnumType $ last args
  [ RsFunction
      (RsIdent name)
      arguments
      return
      (RsBlock [RsNoSemi body])
    ]

instance ToRust Definition [RsItem] where
  toRust def
    | defNoCompilation def || not (usableModality $ getModality def) = return []
  toRust def = do
    let f = defName def

    leftoverVariants <- case theDef def of
      Constructor {} -> return []
      _ -> do
        variants <- gets toRustEnums

        return [RsEnum (RsIdent "Test") variants]

    rustDefinition <- case theDef def of
      Axiom {} -> do
        --        f' <- newRustDef f
        return []
      GeneralizableVar {} -> return []
      Function {} -> do
        strat <- getEvaluationStrategy
        maybeCompiled <- liftTCM $ toTreeless strat f
        case maybeCompiled of
          Just tl -> do
            body <- toRust tl
            return (compileFunction def tl body)
          Nothing -> return []
      Primitive {} -> return []
      PrimitiveSort {} -> return []
      Datatype {dataCons = cons, dataMutual = mut} -> do
        let name = RsIdent (getDataTypeName (head cons))

        variantNames <- mapM makeRustName cons
        variants <- mapM (\x -> return (RsVariant x [])) variantNames

        return [RsEnum name variants]
      Record {} -> return []
      Constructor {conSrcCon = chead, conArity = nargs} -> do return []
      --constructorName <- makeRustName (conName chead)

      --addRustEnumVariant (RsVariant constructorName [])

      --let dataTypeName = RsIdent (getDataTypeName (conName chead))
      --let enumName = Just (RsEnumType dataTypeName)
      --let args = [] -- [RsArgument (RsIdent "x") (RsEnumType (RsIdent "Bool"))]
      --let body = RsBlock [RsNoSemi (RsDataConstructor dataTypeName constructorName [])]

      --return [RsFunction constructorName (RsFunctionDecl args enumName) body]
      AbstractDefn {} -> __IMPOSSIBLE__
      DataOrRecSig {} -> __IMPOSSIBLE__

    return ([] ++ rustDefinition)

-- return (leftoverVariants ++ rustDefinition)

instance ToRust TTerm RsExpr where
  toRust v = do
    toRust $ tAppView v

instance ToRust (TTerm, [TTerm]) RsExpr where
  toRust (TCoerce w, args) = toRust (w, args)
  toRust (TApp w args1, args2) = toRust (w, args1 ++ args2)
  toRust (w, args) = do
    -- args <- traverse toRust args
    case w of
      TVar i -> do
        name <- getVarName i
        return (RsVarRef name)
      TPrim p -> error ("Not implemented " ++ show w)
      TDef d -> do
        name <- makeRustName d
        return (RsVarRef name)
      TLam v -> withFreshVar $ \x -> do
        body <- toRust v
        return (RsClosure [RsIdent x] body)
      TLit l -> error ("Not implemented " ++ show w)
      TCon c -> do
        name <- toRust c
        return (RsFunctionCall name [])
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
    return (RsArm (RsDataConstructor (RsIdent (getDataTypeName c)) c' []) result)
  toRust TAGuard {} = __IMPOSSIBLE__
  toRust TALit {} = __IMPOSSIBLE__

instance ToRust QName RsIdent where
  toRust n = do makeRustName n
