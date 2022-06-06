module Agda.Compiler.ToRust where

import           Agda.Compiler.Backend                           (HasConstInfo (getConstInfo),
                                                                  TTerm)
import           Agda.Compiler.Common
import           Agda.Compiler.Hir
import           Agda.Compiler.ToTreeless                        (toTreeless)
import           Agda.Compiler.Treeless.EliminateLiteralPatterns
import           Agda.Compiler.Treeless.GuardsToPrims
import           Agda.Compiler.Treeless.NormalizeNames           (normalizeNames)
import           Agda.Syntax.Abstract.Name
import           Agda.Syntax.Common
import           Agda.Syntax.Concrete                            (Name (nameNameParts))
import           Agda.Syntax.Internal                            as I
import           Agda.Syntax.Literal
import           Agda.Syntax.Parser.Literate                     (literateMd)
import           Agda.Syntax.Treeless
import           Agda.TypeChecking.Monad
import           Agda.TypeChecking.Pretty
import           Agda.TypeChecking.Primitive.Base
import           Agda.TypeChecking.Substitute                    (TelV (theCore, theTel))
import           Agda.TypeChecking.Telescope                     (telView)
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
    , toRustDefs      :: Map QName HirIdent
    , toRustUsedNames :: Set HirIdent
    }

data ToRustEnv =
  ToRustEnv
    { toRustOptions :: RustOptions
    , toRustVars    :: [(HirIdent, Bool)]
    }

type ToRustM a = StateT ToRustState (ReaderT ToRustEnv TCM) a

initToRustEnv :: RustOptions -> ToRustEnv
initToRustEnv opts = ToRustEnv opts []

addBinding :: HirIdent -> Bool -> ToRustEnv -> ToRustEnv
addBinding x shouldDeref env =
  env {toRustVars = (x, shouldDeref) : toRustVars env}

getVar :: Int -> ToRustM (HirIdent, Bool)
getVar i = reader $ (!! i) . toRustVars

reservedNames :: Set HirIdent
reservedNames =
  Set.fromList ["if", "fn", "match", "+", "-", "*", "/", "true", "false"]

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

isNameUsed :: HirIdent -> ToRustM Bool
isNameUsed x = gets (Set.member x . toRustUsedNames)

setNameUsed :: HirIdent -> ToRustM ()
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
makeRustName :: QName -> ToRustM HirIdent
makeRustName n = go $ T.pack $ fixName $ prettyShow $ qnameName n
  where
    nextName x = T.pack ('z' : T.unpack x) -- TODO: do something smarter
    go s = ifM (isNameUsed s) (go $ nextName s) (return s)
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
  local (addBinding x shouldDeref) $ f x

freshRustIdentifier :: ToRustM Text
freshRustIdentifier = do
  names <- gets toRustFresh
  case names of
    [] -> fail "No more variables!"
    (ident:names') -> do
      modify $ \st -> st {toRustFresh = names'}
      ifM (isNameUsed ident) freshRustIdentifier $ {-otherwise-}
       do
        setNameUsed ident
        return ident

getFunctionName :: QName -> Text
getFunctionName = replace "." "_" . T.pack . prettyShow

getGenericTypes :: Term -> [HirType]
getGenericTypes (Pi _ abs) = [HirGeneric $ T.pack $ absName abs]
getGenericTypes _          = []

instance ToRust Type HirType where
  toRust term =
    case unEl term of
      Sort _ -> return HirNone
      Var n _ -> return $ HirBruijn n
      Def name _
        | length (prettyShow $ qnameName name) /= 1 -> do
          constInfo <- liftTCM $ getConstInfo name
          let genericTypes = getGenericTypes $ unEl $ defType constInfo
          return $
            HirNamedType (T.pack $ prettyShow $ qnameName name) genericTypes
      Def name _ -> return $ HirGeneric $ T.pack $ prettyShow $ qnameName name
      Pi dom abs -> do
        first <- toRust $ unDom dom
        rest <- toRust $ unAbs abs
        return $ HirFn first rest
      _ ->
        return $
        trace
          ("NOT IMPLEMENTED " ++ show term ++ "\t\t" ++ prettyShow term)
          HirNone

eliminateDeBruijn :: Int -> [HirType] -> [HirType]
eliminateDeBruijn offset xs =
  zipWith
    (\x i ->
       (case x of
          (HirBruijn j) ->
            HirNamedType (T.pack [['A' ..] !! (offset + i - j - 1)]) []
          (HirFn a b) ->
            HirFn
              (head $ eliminateDeBruijn i [a])
              (head $ eliminateDeBruijn i [b])
          _ -> x))
    xs
    [0 ..]

unpackTele :: I.Tele (I.Dom Type) -> [Type]
unpackTele EmptyTel         = []
unpackTele (ExtendTel x xs) = unDom x : unpackTele (unAbs xs)

getSignatureFromDef :: Definition -> ToRustM [HirType]
getSignatureFromDef def = do
  let t = defType def
  telView <- telView t
  arguments <- mapM toRust (unpackTele $ theTel telView)
  ret <- toRust $ theCore telView
  return $ eliminateDeBruijn 0 (arguments ++ [ret])

compileFunction :: Definition -> TTerm -> HirExpr -> ToRustM [HirStmt]
compileFunction func tl body = do
  let def = theDef func
  name <- makeRustName $ defName func
  args <- getSignatureFromDef func
  return [HirFunction name args body]

unique :: Eq a => [a] -> [a]
unique []     = []
unique (x:xs) = x : unique (filter (x /=) xs)

instance ToRust Definition [HirStmt] where
  toRust def
    | defNoCompilation def || not (usableModality $ getModality def) = return []
  toRust def =
    case theDef def of
      Axiom {}
        --        f' <- newRustDef f
       -> return []
      GeneralizableVar {} -> return []
      Function {} -> do
        strat <- getEvaluationStrategy
        maybeCompiled <- liftTCM $ toTreeless strat (defName def)
        case maybeCompiled of
          Just tl -> do
            body <- toRust tl
            trace (prettyShow tl) (compileFunction def tl body)
          Nothing -> return []
      Primitive {} -> return []
      PrimitiveSort {} -> return []
      Datatype {dataCons = cons, dataMutual = mut} -> do
        let enumName = getDataTypeName $ head cons
        variantNames <- mapM makeRustName cons
        signatures <- mapM (liftTCM . getConstInfo) cons
        constructorFnTypes <- mapM getSignatureFromDef signatures
        constructorNames <- mapM makeRustName cons
        return
          [HirConstructor enumName (zip constructorNames constructorFnTypes)]
      Record {} -> return []
      Constructor {conSrcCon = chead, conArity = nargs} -> return []
      AbstractDefn {} -> __IMPOSSIBLE__
      DataOrRecSig {} -> __IMPOSSIBLE__

instance ToRust TTerm HirExpr where
  toRust v = do
    v <- liftTCM $ eliminateLiteralPatterns (convertGuards v)
    toRust $ tAppView v

derefIfRequired :: HirExpr -> Bool -> HirExpr
derefIfRequired expr False = expr
derefIfRequired expr True  = HirDeref expr

instance ToRust (TTerm, [TTerm]) HirExpr where
  toRust (TCoerce w, args) = toRust (w, args)
  toRust (TApp w args1, args2) = toRust (w, args1 ++ args2)
  toRust (w, args) = do
    args <- traverse toRust args
    case w of
      TVar i -> do
        (name, shouldDeref) <- getVar i
        case args of
          []   -> return $ derefIfRequired (HirVarRef name) shouldDeref
          args -> return $ HirClosureCall name args
      TPrim p -> toRust (p, args)
      TDef d -> do
        name <- makeRustName d
        return (HirFnCall name args)
      TLam v ->
        withFreshVar False $ \x -> do
          body <- toRust v
          return (HirClosure x body)
      TLit l -> do
        unless (null args) __IMPOSSIBLE__
        toRust l
      TCon c -> do
        name <- makeRustName c
        return (HirFnCall name args)
      TLet u v -> do
        expr <- toRust u
        withFreshVar False $ \x -> do
          body <- toRust v
          return $ HirLet x expr body
      TCase i info v bs -> do
        cases <- traverse toRust bs
        (var, shouldDeref) <- getVar i
        let matchClause = derefIfRequired (HirVarRef var) shouldDeref
        fallback <-
          if isUnreachable v
            then return Nothing
            else Just <$> toRust v
        return (HirMatch matchClause cases fallback)
      TUnit -> error ("Not implemented " ++ show w)
      TSort -> error ("Not implemented " ++ show w)
      TErased -> return HirNoneInstance
      TError err -> error ("Not implemented " ++ show w)

instance ToRust (TPrim, [HirExpr]) HirExpr where
  toRust (PAdd, args) = error "Not implemented" -- return $ HirBinop "+" (head args) (args !! 1)
  toRust (PSub, args) = error "Not implemented" -- return $ HirBinop "-" (head args) (args !! 1)
  toRust (PIf, args)  = error "Not implemented" -- return $ HirIfElse (head args) (args !! 1) (args !! 2)
  toRust (PEqI, args) = error "Not implemented" -- return $ HirBinop "==" (head args) (args !! 1)
  toRust x            = error "Not implemented "

instance ToRust Literal HirExpr where
  toRust lit =
    case lit of
      LitNat x    -> error ("Not implemented " ++ show lit)
      LitWord64 x -> error ("Not implemented " ++ show lit)
      LitFloat x  -> error ("Not implemented " ++ show lit)
      LitString x -> error ("Not implemented " ++ show lit)
      LitChar x   -> error ("Not implemented " ++ show lit)
      LitQName x  -> error ("Not implemented " ++ show lit)
      LitMeta p x -> error ("Not implemented " ++ show lit)

instance ToRust TAlt HirArm where
  toRust (TACon c nargs v) = do
    constInfo <- liftTCM $ getConstInfo c
    types <- getSignatureFromDef constInfo
    withFreshVars (length types - 1) True $ \xs -> do
      c' <- makeRustName c
      body <- toRust v
      return
        (HirDataConstructor (getDataTypeName c) c' (map HirVarRef xs), body)
  toRust TAGuard {} = __IMPOSSIBLE__
  toRust (TALit lit body) = do
    lit <- toRust lit
    body <- toRust body
    return (lit, body)
