module Agda.Compiler.HirToLir where

import           Agda.Compiler.Hir
import           Agda.Compiler.Lir
import           Agda.Utils.Impossible (__IMPOSSIBLE__)
import           Data.Bifunctor
import qualified Data.Text             as T
import           Debug.Trace

class ToLir a b where
  toLir :: a -> b

instance ToLir HirExpr LirExpr where
  toLir (HirVarRef x) = LirVarRef x
  toLir (HirDataConstructor datatype constructor args) =
    LirDataConstructor datatype constructor (map toLir args)
  toLir (HirFnCall name args) = LirFnCall name (map toLir args)
  toLir (HirClosure arg body) = LirClosure arg (toLir body)
  toLir (HirLet name expr body) = LirLet name (toLir expr) (toLir body)
  toLir (HirMatch expr arms fallback) =
    LirMatch
      (toLir expr)
      (map (bimap toLir toLir) arms ++ [(LirWildcard, LirUnreachable)])
      (fmap toLir fallback)
  toLir (HirDeref expr) = LirDeref (toLir expr)
  toLir HirNoneInstance = LirNoneInstance

instance ToLir HirType LirType where
  toLir (HirNamedType name generics) = LirNamedType name (map toLir generics)
  toLir (HirGeneric name)            = LirGeneric name
  toLir (HirBruijn _)                = __IMPOSSIBLE__
  toLir (HirFn argType retType)      = LirFn (toLir argType) (toLir retType)
  toLir HirNone                      = LirNone

unique :: Eq a => [a] -> [a]
unique []     = []
unique (x:xs) = x : unique (filter (x /=) xs)

extractGenericsFromType :: LirType -> [LirType]
extractGenericsFromType (LirNamedType name gs) =
  unique gs ++ [LirGeneric name | T.length name == 1]
extractGenericsFromType x@(LirGeneric _) = [x]
extractGenericsFromType (LirFn arg ret) =
  unique (extractGenericsFromType arg ++ extractGenericsFromType ret)
extractGenericsFromType (LirBoxed t) = extractGenericsFromType t
extractGenericsFromType LirNone = []

extractGenericsFromStmt :: LirStmt -> [LirType]
extractGenericsFromStmt (LirFunction _ generics ret _) =
  generics ++ extractGenericsFromType ret
extractGenericsFromStmt (LirEnum _ gs variants) =
  unique $
  concat
    (map extractGenericsFromType gs ++
     concatMap (\(_, xs) -> map extractGenericsFromType xs) variants)
extractGenericsFromStmt (LirTypeAlias _ t g) = extractGenericsFromType t ++ g

instance ToLir [HirStmt] [LirStmt] where
  toLir = concatMap toLir

removeLast :: [a] -> [a]
removeLast xs = [xs !! i | i <- [0 .. (length xs - 2)]]

-- Convert function to lir
instance ToLir (LirIdent, [LirType], LirExpr) [LirStmt] where
  toLir (name, [argType], body) = do
    let makeAliasName name n = T.append name (T.pack $ show n)
    [LirFunction name (extractGenericsFromType argType) argType body]
  toLir (name, argTypes, body) = do
    let makeAliasName name n = T.append name (T.pack $ show n)
    let firstType = LirFn (argTypes !! (length argTypes - 2)) (last argTypes)
    let types =
          firstType :
          zipWith
            (\t n ->
               let generics =
                     unique
                       (extractGenericsFromType firstType ++
                        extractGenericsFromType (argTypes !! (n - 1)))
                in LirFn t (LirNamedType (makeAliasName name (n - 2)) generics))
            (reverse $ removeLast $ removeLast argTypes)
            [2 ..]
    let typeAliases =
          zipWith
            (\t n ->
               LirTypeAlias (makeAliasName name n) t (extractGenericsFromType t))
            types
            [0 ..]
    let returnGenerics = unique $ concatMap extractGenericsFromStmt typeAliases
    let returnType =
          LirNamedType
            (makeAliasName name (length typeAliases - 1))
            returnGenerics
    typeAliases ++
      [LirFunction name (extractGenericsFromType returnType) returnType body]

instance ToLir HirStmt [LirStmt] where
  toLir (HirFunction name argTypes body) = do
    let lirBody :: LirExpr = toLir body
    let lirArgTypes :: [LirType] = map toLir argTypes
    toLir (name, lirArgTypes, lirBody)
  toLir (HirConstructor name variants) = do
    let lirVariants =
          map (\(n, args) -> (n, map toLir (removeLast args))) variants
    let generics =
          unique $
          concatMap (\(_, x) -> concatMap extractGenericsFromType x) lirVariants
    let enumConstructor =
          LirEnum name generics (map (second (map LirBoxed)) lirVariants)
    let constructorBodies =
          map
            (\(n, ts) ->
               foldr
                 (\(x, argName) acc -> LirClosure (T.pack [argName]) acc)
                 (LirDataConstructor
                    name
                    n
                    (map
                       (\c -> LirBox $ LirVarRef $ T.pack [c])
                       (take (length ts) ['a' ..])))
                 (zip ts ['a' ..]))
            lirVariants
    let constructorFunctions =
          concat $
          zipWith
            (\(n, ts) body ->
               toLir (n, ts ++ [LirNamedType name generics], body))
            lirVariants
            constructorBodies
    enumConstructor : constructorFunctions
