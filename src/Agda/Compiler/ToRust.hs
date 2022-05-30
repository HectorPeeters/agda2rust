module Agda.Compiler.ToRust where

import           Agda.Compiler.Backend                           (TTerm)
import           Agda.Compiler.Common
import           Agda.Compiler.RustSyntax
import           Agda.Compiler.ToTreeless                        (toTreeless)
import           Agda.Compiler.Treeless.EliminateLiteralPatterns
import           Agda.Compiler.Treeless.GuardsToPrims
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
import           Data.List                                       hiding (null)
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
    , toRustVars    :: [(RsIdent, Bool)]
    }

type ToRustM a = StateT ToRustState (ReaderT ToRustEnv TCM) a

initToRustEnv :: RustOptions -> ToRustEnv
initToRustEnv opts = ToRustEnv opts []

addBinding :: RsIdent -> Bool -> ToRustEnv -> ToRustEnv
addBinding x shouldDeref env =
  env {toRustVars = (x, shouldDeref) : toRustVars env}

getVar :: Int -> ToRustM (RsIdent, Bool)
getVar i = reader $ (!! i) . toRustVars

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
  | isAscii x = isAlphaNum x || x == '_'
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

withFreshVar :: Bool -> (Text -> ToRustM a) -> ToRustM a
withFreshVar shouldDeref f = do
  strat <- getEvaluationStrategy
  withFreshVar' strat shouldDeref f

withFreshVars :: Int -> Bool -> ([Text] -> ToRustM a) -> ToRustM a
withFreshVars i shouldDeref f
  | i <= 0 = f []
  | otherwise =
    withFreshVar shouldDeref $ \x ->
      withFreshVars (i - 1) shouldDeref (f . (x :))

withFreshVar' :: EvaluationStrategy -> Bool -> (Text -> ToRustM a) -> ToRustM a
withFreshVar' strat shouldDeref f = do
  x <- freshRustIdentifier
  local (addBinding (RsIdent x) shouldDeref) $ f x

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

getFunctionName :: QName -> Text
getFunctionName = replace "." "_" . T.pack . prettyShow

removeLastItem :: [a] -> [a]
removeLastItem []     = []
removeLastItem [x]    = []
removeLastItem (x:xs) = x : removeLastItem xs

extractTypes :: Term -> ToRustM [RsType]
extractTypes x =
  case x of
    Sort _ -> return [RsNone]
    Var n _ -> do
      return [RsEnumType (RsIdent $ T.pack [['A' ..] !! n]) []]
    Def name _ -> do
      constInfo <- liftTCM $ getConstInfo name
      -- TODO: determine if we need generic arguments here
      return [RsEnumType (RsIdent $ T.pack $ prettyShow $ qnameName name) []]
    Pi dom abs -> do
      first <- extractTypes $ unEl $ unDom dom
      rest <- extractTypes $ unEl $ unAbs abs
      return (first ++ rest)
    _ ->
      return $ trace ("NOT IMPLEMENTED " ++ show x ++ "\t\t" ++ prettyShow x) []

compileFunction :: Definition -> TTerm -> RsExpr -> ToRustM [RsItem]
compileFunction func tl body = do
  let def = theDef func
  name <- makeRustName $ defName func
  args <- extractTypes $ unEl $ defType func
  let arguments = removeLastItem args
  let ret = Just $ last args
  return [RsFunction name arguments ret (RsBlock [RsNoSemi body])]

instance ToRust Definition [RsItem] where
  toRust def
    | defNoCompilation def || not (usableModality $ getModality def) = return []
  toRust def = do
    case theDef def of
      Axiom {}
        --        f' <- newRustDef f
       -> do
        return []
      GeneralizableVar {} -> return []
      Function {} -> do
        strat <- getEvaluationStrategy
        maybeCompiled <- liftTCM $ toTreeless strat (defName def)
        case maybeCompiled of
          Just tl -> do
            body <- toRust tl
            compileFunction def tl body
          Nothing -> return []
      Primitive {} -> return []
      PrimitiveSort {} -> return []
      Datatype {dataCons = cons, dataMutual = mut} -> do
        let enumName = RsIdent (getDataTypeName (head cons))
        variantNames <- mapM makeRustName cons
        signatures <- mapM (liftTCM . getConstInfo) cons
        fullSignatures <- mapM (extractTypes . unEl . defType) signatures
        let constructorFnTypes =
              map (\x -> (last x, removeLastItem x)) fullSignatures
        constructorNames <- mapM makeRustName cons
        -- NOTE: don't look at the following few lines of code. At least it works
        let rustFunctions =
              zipWith
                (curry
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
                                     enumName
                                     n
                                     (map
                                        (\x ->
                                           RsBox $
                                           RsVarRef $ RsIdent $ T.pack (x : ""))
                                        (take (length ts) ['a' .. 'z'])))
                                  (zip ts ['a' .. 'z']))
                           ])))
                constructorNames
                constructorFnTypes
        let allGenericTypes =
              filter
                (\x -> length (show x) == 1)
                (unique $ concatMap snd constructorFnTypes)
        let variants =
              zipWith
                (curry
                   (\(n, (h, ts)) ->
                      RsVariant
                        n
                        (map
                           (\x ->
                              case x of
                                RsEnumType n _
                                  | n == enumName ->
                                    RsBoxed $
                                    RsEnumType enumName allGenericTypes
                                _ -> RsBoxed x)
                           ts)))
                variantNames
                constructorFnTypes
        return (RsEnum enumName allGenericTypes variants : rustFunctions)
      Record {} -> return []
      Constructor {conSrcCon = chead, conArity = nargs} -> do
        return []
      AbstractDefn {} -> __IMPOSSIBLE__
      DataOrRecSig {} -> __IMPOSSIBLE__

instance ToRust TTerm RsExpr where
  toRust v = do
    v <- liftTCM $ eliminateLiteralPatterns (convertGuards v)
    toRust $ tAppView v

derefIfRequired :: RsExpr -> Bool -> RsExpr
derefIfRequired expr False = expr
derefIfRequired expr True  = RsDeref expr

instance ToRust (TTerm, [TTerm]) RsExpr where
  toRust (TCoerce w, args) = toRust (w, args)
  toRust (TApp w args1, args2) = toRust (w, args1 ++ args2)
  toRust (w, args) = do
    args <- traverse toRust args
    case w of
      TVar i -> do
        (name, shouldDeref) <- getVar i
        return $ derefIfRequired (RsVarRef name) shouldDeref
      TPrim p -> toRust (p, args)
      TDef d -> do
        name <- makeRustName d
        return (RsFunctionCall name args)
      TLam v ->
        withFreshVar False $ \x -> do
          body <- toRust v
          return (RsClosure [RsIdent x] body)
      TLit l -> do
        unless (null args) __IMPOSSIBLE__
        toRust l
      TCon c -> do
        name <- makeRustName c
        return (RsFunctionCall name args)
      TLet u v -> do
        expr <- toRust u
        withFreshVar False $ \x -> do
          body <- toRust v
          return $ RsLet (RsIdent x) expr body
      TCase i info v bs -> do
        cases <- traverse toRust bs
        (var, shouldDeref) <- getVar i
        let matchClause = derefIfRequired (RsVarRef var) shouldDeref
        fallback <-
          if isUnreachable v
            then return Nothing
            else Just <$> toRust v
        return (RsMatch matchClause cases fallback)
      TUnit -> error ("Not implemented " ++ show w)
      TSort -> error ("Not implemented " ++ show w)
      TErased -> return RsNoneInstance
      TError err -> error ("Not implemented " ++ show w)

instance ToRust (TPrim, [RsExpr]) RsExpr where
  toRust (PAdd, args) = do
    return $ RsBinop "+" (args !! 0) (args !! 1)
  toRust (PSub, args) = do
    return $ RsBinop "-" (args !! 0) (args !! 1)
  toRust (PIf, args) = do
    return $ RsIfElse (args !! 0) (args !! 1) (args !! 2)
  toRust (PEqI, args) = do
    return $ RsBinop "==" (args !! 0) (args !! 1)
  toRust x = error ("Not implemented " ++ show x)

instance ToRust Literal RsExpr where
  toRust lit =
    case lit of
      LitNat x    -> return $ RsIntLit x
      LitWord64 x -> error ("Not implemented " ++ show lit)
      LitFloat x  -> error ("Not implemented " ++ show lit)
      LitString x -> error ("Not implemented " ++ show lit)
      LitChar x   -> error ("Not implemented " ++ show lit)
      LitQName x  -> error ("Not implemented " ++ show lit)
      LitMeta p x -> error ("Not implemented " ++ show lit)

instance ToRust TAlt RsArm where
  toRust (TACon c nargs v) = do
    constInfo <- liftTCM $ getConstInfo c
    types <- extractTypes $ unEl $ defType constInfo
    withFreshVars (length types - 1) True $ \xs -> do
      c' <- makeRustName c
      body <- toRust v
      return
        (RsArm
           (RsDataConstructor
              (RsIdent (getDataTypeName c))
              c'
              (map (RsVarRef . RsIdent) xs))
           body)
  toRust TAGuard {} = __IMPOSSIBLE__
  toRust TALit {} = __IMPOSSIBLE__
