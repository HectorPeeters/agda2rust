module Agda.Compiler.ToRust where

import           Agda.Compiler.Backend                           (TTerm)
import           Agda.Compiler.Common
import           Agda.Compiler.RustSyntax
import           Agda.Compiler.ToTreeless                        (toTreeless)
import           Agda.Compiler.Treeless.EliminateLiteralPatterns
import           Agda.Compiler.Treeless.NormalizeNames           (normalizeNames)
import           Agda.Syntax.Abstract.Name
import           Agda.Syntax.Common
import           Agda.Syntax.Concrete                            (Name (nameNameParts))
import           Agda.Syntax.Internal                            as I
import           Agda.Syntax.Literal
import           Agda.Syntax.Treeless
import           Agda.TypeChecking.Monad
import           Agda.TypeChecking.Pretty
import           Agda.TypeChecking.Primitive.Base
import           Agda.Utils.Impossible                           (__IMPOSSIBLE__)
import           Agda.Utils.Lens
import           Agda.Utils.List
import           Agda.Utils.Maybe
import           Agda.Utils.Monad                                (ifM, unlessM)
import           Agda.Utils.Null
import           Agda.Utils.Pretty
import qualified Agda.Utils.Pretty                               as P
import           Agda.Utils.Singleton
import           Control.DeepSeq                                 (NFData)
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.IO.Class                          (MonadIO (liftIO))
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Char
import           Data.Data                                       (dataTypeName)
import           Data.List
import           Data.Map                                        (Map)
import qualified Data.Map                                        as Map
import           Data.Set                                        (Set)
import qualified Data.Set                                        as Set
import           Data.Text                                       (Text, replace)
import qualified Data.Text                                       as T
import           Debug.Trace                                     (trace)
import           GHC.Generics                                    (Generic)
import           Prelude                                         hiding (empty,
                                                                  null)

deriving instance Generic EvaluationStrategy

deriving instance NFData EvaluationStrategy

newtype RustOptions =
  RustOptions
    { rustEvaluation :: EvaluationStrategy
    }
  deriving (Generic, NFData)

data ToRustState =
  ToRustState
    { toRustFresh     :: [Text]
    , toRustDefs      :: Map QName RsIdent
    , toRustUsedNames :: Set RsIdent
    }

data ToRustEnv =
  ToRustEnv
    { toRustOptions :: RustOptions
    , toRustVars    :: [RsIdent]
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
  map RsIdent ["if", "fn", "match", "+", "-", "*", "/", "true", "false"]

freshVars :: [Text]
freshVars = concat [map (<> i) xs | i <- "" : map (T.pack . show) [1 ..]]
  where
    xs = map T.singleton ['a' .. 'z']

initToRustState :: ToRustState
initToRustState =
  ToRustState
    { toRustFresh = freshVars
    , toRustDefs = Map.empty
    , toRustUsedNames = reservedNames
    }

runToRustM :: RustOptions -> ToRustM a -> TCM a
runToRustM opts =
  (`runReaderT` initToRustEnv opts) . (`evalStateT` initToRustState)

getEvaluationStrategy :: ToRustM EvaluationStrategy
getEvaluationStrategy = reader $ rustEvaluation . toRustOptions

class ToRust a b where
  toRust :: a -> ToRustM b

isNameUsed :: RsIdent -> ToRustM Bool
isNameUsed x = gets (Set.member x . toRustUsedNames)

setNameUsed :: RsIdent -> ToRustM ()
setNameUsed x =
  modify $ \s -> s {toRustUsedNames = Set.insert x (toRustUsedNames s)}

rustAllowedUnicodeCats :: Set GeneralCategory
rustAllowedUnicodeCats =
  Set.fromList
    [ UppercaseLetter
    , LowercaseLetter
    , TitlecaseLetter
    , ModifierLetter
    , OtherLetter
    , NonSpacingMark
    , SpacingCombiningMark
    , EnclosingMark
    , DecimalNumber
    , LetterNumber
    , OtherNumber
    , ConnectorPunctuation
    , DashPunctuation
    , OtherPunctuation
    , CurrencySymbol
    , MathSymbol
    , ModifierSymbol
    , OtherSymbol
    , PrivateUse
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
       in if isNumber (head s')
            then "z" ++ s'
            else s'
    fixChar c
      | isValidRustChar c = [c]
      | otherwise = "\\x" ++ toHex (ord c) ++ ";"
    toHex 0 = ""
    toHex i = toHex (i `div` 16) ++ [fourBitsToChar (i `mod` 16)]

getDataTypeName :: QName -> Text
getDataTypeName name =
  T.pack $ prettyShow (nameConcrete (last (mnameToList (qnameModule name))))

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
    (x:names') -> do
      let ident = RsIdent x
      modify $ \st -> st {toRustFresh = names'}
      ifM (isNameUsed ident) freshRustIdentifier $ {-otherwise-}
       do
        setNameUsed ident
        return x

generateFunctionName :: QName -> Text
generateFunctionName = replace "." "_" . T.pack . prettyShow

removeLastItem :: [a] -> [a]
removeLastItem []     = []
removeLastItem [x]    = []
removeLastItem (x:xs) = x : removeLastItem xs

extractTypes :: Term -> [RsType]
extractTypes x =
  case x of
    Sort _ -> [RsNone]
    Var n _ -> [RsEnumType $ RsIdent $ T.pack [['A' ..] !! n]]
    Def name _ -> [RsEnumType $ RsIdent $ T.pack $ prettyShow $ qnameName name]
    Pi dom abs -> do
      let first = extractTypes $ unEl $ unDom dom
      let rest = extractTypes $ unEl $ unAbs abs
      first ++ rest
    _ -> trace ("NOT IMPLEMENTED " ++ show x ++ "\t\t" ++ prettyShow x) []

compileFunction :: Definition -> TTerm -> RsExpr -> [RsItem]
compileFunction func tl body = do
  let def = theDef func
  let name = generateFunctionName $ defName func
  let args = extractTypes $ unEl $ defType func
  let arguments = removeLastItem args
  let return = Just $ last args
  [RsFunction (RsIdent name) arguments return (RsBlock [RsNoSemi body])]

instance ToRust Definition [RsItem] where
  toRust def
    | defNoCompilation def || not (usableModality $ getModality def) = return []
  toRust def = do
    let f = defName def
    rustDefinition <-
      case theDef def of
        Axiom {}
        --        f' <- newRustDef f
         -> do
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
          signatures <- mapM (\c -> liftTCM $ getConstInfo c) cons
          let fullSignatures =
                map (\s -> extractTypes $ unEl $ defType s) signatures
          let constructorFnTypes =
                map (\x -> (head x, take (length x - 1) x)) fullSignatures
          constructorNames <- mapM makeRustName cons
          -- NOTE: don't look at the following few lines of code. At least it works
          let rustFunctions =
                map
                  (\(n, (h, ts)) ->
                     RsFunction
                       n
                       ts
                       (Just h)
                       (RsBlock
                          [ RsNoSemi
                              (foldr
                                 (\(x, i) acc ->
                                    RsClosure [RsIdent $ T.pack (i : "")] acc)
                                 (RsDataConstructor
                                    name
                                    n
                                    (map
                                       (\x ->
                                          RsBox $
                                          RsVarRef $ RsIdent $ T.pack (x : ""))
                                       (take (length ts) ['a' .. 'z'])))
                                 (zip ts ['a' .. 'z']))
                          ]))
                  (zip constructorNames constructorFnTypes)
          let variants =
                map
                  (\(x, (h, ts)) -> RsVariant x (map (\x -> RsBoxed x) ts))
                  (zip variantNames constructorFnTypes)
          return ((RsEnum name variants) : rustFunctions)
        Record {} -> return []
        Constructor {conSrcCon = chead, conArity = nargs} -> do
          return []
        AbstractDefn {} -> __IMPOSSIBLE__
        DataOrRecSig {} -> __IMPOSSIBLE__
    return rustDefinition

instance ToRust TTerm RsExpr where
  toRust v = do
    toRust $ tAppView v

instance ToRust (TTerm, [TTerm]) RsExpr where
  toRust (TCoerce w, args) = toRust (w, args)
  toRust (TApp w args1, args2) = toRust (w, args1 ++ args2)
  toRust (w, args)
    -- args <- traverse toRust args
   = do
    case w of
      TVar i -> do
        name <- getVarName i
        return (RsVarRef name)
      TPrim p -> error ("Not implemented " ++ show w)
      TDef d -> do
        name <- makeRustName d
        return (RsVarRef name)
      TLam v ->
        withFreshVar $ \x -> do
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
    return
      (RsArm (RsDataConstructor (RsIdent (getDataTypeName c)) c' []) result)
  toRust TAGuard {} = __IMPOSSIBLE__
  toRust TALit {} = __IMPOSSIBLE__

instance ToRust QName RsIdent where
  toRust n = do
    makeRustName n
