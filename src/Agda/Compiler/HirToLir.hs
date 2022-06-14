module Agda.Compiler.HirToLir where

import           Agda.Auto.NarrowingSearch (extractblkinfos)
import           Agda.Compiler.Backend     (EvaluationStrategy (LazyEvaluation))
import           Agda.Compiler.Hir
import           Agda.Compiler.Lir
import           Agda.Utils.Impossible     (__IMPOSSIBLE__)
import           Data.Bifunctor
import qualified Data.Text                 as T
import           Debug.Trace

class ToLir a b where
  toLir :: a -> EvaluationStrategy -> b

needsLazyFake :: LirExpr -> Bool
needsLazyFake (LirVarRef _) = True
needsLazyFake (LirEnumConstructor _ _ args) =
  trace (show args) (any needsLazyFake args)
needsLazyFake (LirFnCall _ args) = any needsLazyFake args
needsLazyFake (LirClosureCall _ args) = any needsLazyFake args
needsLazyFake (LirLazyConstructor _ x) = x
needsLazyFake (LirClone expr) = needsLazyFake expr
needsLazyFake (LirLet _ value body) = needsLazyFake value || needsLazyFake body
needsLazyFake (LirMatch clause arms Nothing) =
  needsLazyFake clause || any (needsLazyFake . snd) arms
needsLazyFake (LirMatch clause arms (Just fallback)) =
  needsLazyFake clause ||
  any (needsLazyFake . snd) arms || needsLazyFake fallback
needsLazyFake (LirDeref expr) = needsLazyFake expr
needsLazyFake (LirBox expr) = needsLazyFake expr
needsLazyFake _ = False

instance ToLir HirExpr LirExpr where
  toLir (HirVarRef x) _ = LirVarRef x
  toLir (HirDataConstructor datatype constructor args) strat =
    LirEnumConstructor datatype constructor (map (`toLir` strat) args)
  toLir (HirFnCall name args) strat =
    LirFnCall
      name
      if strat == LazyEvaluation
        then map
               (\x ->
                  (let l = toLir x strat
                    in LirLazyConstructor l (needsLazyFake l)))
               args
        else map (`toLir` strat) args
  toLir (HirClosureCall name args) strat =
    LirClosureCall name (map (`toLir` strat) args)
  toLir (HirClosure arg body) strat = LirClosure [arg] (toLir body strat)
  toLir (HirClone expr) strat = LirClone (toLir expr strat)
  toLir (HirLet name expr body) strat =
    LirLet name (toLir expr strat) (toLir body strat)
  toLir (HirMatch expr arms fallback) strat =
    LirMatch
      (toLir expr strat)
      (map (bimap (`toLir` strat) (`toLir` strat)) arms ++
       [(LirWildcard, LirUnreachable)])
      (fmap (`toLir` strat) fallback)
  toLir (HirDeref expr) strat = LirDeref (toLir expr strat)
  toLir HirNoneInstance _ = LirNoneInstance

instance ToLir HirType LirType where
  toLir (HirNamedType name generics) strat =
    LirNamedType name (map (`toLir` strat) generics)
  toLir (HirGeneric name) strat = LirGeneric name
  toLir (HirBruijn _) strat = __IMPOSSIBLE__
  toLir (HirFn argType retType) strat =
    LirFnOnce (toLir argType strat) (toLir retType strat)
  toLir HirNone strat = LirNone

unique :: Eq a => [a] -> [a]
unique []     = []
unique (x:xs) = x : unique (filter (x /=) xs)

extractGenericsFromType :: LirType -> [LirType]
extractGenericsFromType (LirNamedType name gs) =
  concatMap extractGenericsFromType gs ++ [LirGeneric name | T.length name == 1]
extractGenericsFromType x@(LirGeneric _) = [x]
extractGenericsFromType (LirFnOnce arg ret) =
  unique (extractGenericsFromType arg ++ extractGenericsFromType ret)
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
  toLir xs strat = unique $ concatMap (`toLir` strat) xs

removeLast :: [a] -> [a]
removeLast xs = [xs !! i | i <- [0 .. (length xs - 2)]]

-- Convert function to lir
instance ToLir (LirIdent, [LirType], LirExpr) [LirStmt] where
  toLir (name, [argType], body) strat = do
    let makeAliasName name n = T.append name (T.pack $ show n)
    [LirFunction name (extractGenericsFromType argType) argType body]
  toLir (name, argTypes, body) strat = do
    let makeAliasName name n = T.append name (T.pack $ show n)
    let lazy = strat == LazyEvaluation
    let firstType =
          LirFnOnce
            (if lazy
               then LirNamedType "Lazy" [argTypes !! (length argTypes - 2)]
               else argTypes !! (length argTypes - 2))
            (last argTypes)
    let types :: [[LirType]] =
          [firstType] :
          zipWith
            (\typ n ->
               let generics =
                     unique
                       (extractGenericsFromType firstType ++
                        concatMap
                          extractGenericsFromType
                          (take (n + 1) argTypes))
                   ownGenerics = extractGenericsFromType t
                   t =
                     if lazy
                       then LirNamedType "Lazy" [typ]
                       else typ
                in case t of
                     (LirFnOnce arg ret) ->
                       [ LirFn arg ret
                       , LirFnOnce
                           (LirNamedType
                              (T.append (makeAliasName name (n - 1)) "_")
                              (unique (generics ++ ownGenerics)))
                           (LirNamedType (makeAliasName name (n - 2)) generics)
                       ]
                     _ ->
                       [ LirFnOnce
                           t
                           (LirNamedType (makeAliasName name (n - 2)) generics)
                       ])
            (reverse $ removeLast $ removeLast argTypes)
            [2 ..]
    let typeAliases =
          concat
            (zipWith
               (\t n ->
                  case t of
                    [t1] ->
                      [ LirTypeAlias
                          (makeAliasName name n)
                          t1
                          (extractGenericsFromType t1)
                      ]
                    [t1, t2] ->
                      [ LirTypeAlias
                          (T.append (makeAliasName name n) "_")
                          t1
                          (extractGenericsFromType t1)
                      , LirTypeAlias
                          (makeAliasName name n)
                          t2
                          (extractGenericsFromType t2)
                      ])
               types
               [0 ..])
    let returnGenerics = unique $ concatMap extractGenericsFromStmt typeAliases
    let returnType =
          LirNamedType (makeAliasName name (length types - 1)) returnGenerics
    typeAliases ++
      [LirFunction name (extractGenericsFromType returnType) returnType body]

instance ToLir HirStmt [LirStmt] where
  toLir (HirFunction name argTypes body) strat = do
    let lirBody :: LirExpr = toLir body strat
    let lirArgTypes :: [LirType] = map (`toLir` strat) argTypes
    toLir (name, lirArgTypes, lirBody) strat
  toLir (HirConstructor name variants) strat = do
    let lirVariants =
          map
            (\(n, args) -> (n, map (`toLir` strat) (removeLast args)))
            variants
    let generics =
          unique $
          concatMap (\(_, x) -> concatMap extractGenericsFromType x) lirVariants
    let enumConstructor =
          LirEnum name generics (map (second (map LirBoxed)) lirVariants)
    let constructorBodies =
          map
            (\(n, ts) ->
               foldr
                 (\(x, argName) acc -> LirClosure [T.pack [argName]] acc)
                 (LirEnumConstructor
                    name
                    n
                    (map
                       (if strat == LazyEvaluation
                          then (\c -> LirBox $ LirClone $ LirVarRef $ T.pack [c])
                          else (\c -> LirBox $ LirVarRef $ T.pack [c]))
                       (take (length ts) ['a' ..])))
                 (zip ts ['a' ..]))
            lirVariants
    let constructorFunctions =
          concat $
          zipWith
            (\(n, ts) body ->
               toLir (n, ts ++ [LirNamedType name generics], body) strat)
            lirVariants
            constructorBodies
    enumConstructor : constructorFunctions
