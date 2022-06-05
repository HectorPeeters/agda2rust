module Agda.Compiler.HirToLir where

import           Agda.Compiler.Hir
import           Agda.Compiler.Lir
import           Agda.Utils.Impossible (__IMPOSSIBLE__)
import           Data.Bifunctor
import           Debug.Trace

toLir :: HirExpr -> LirExpr
toLir (HirVarRef x) = LirVarRef x
toLir (HirDataConstructor datatype constructor args) =
  LirDataConstructor datatype constructor (map toLir args)
toLir (HirFnCall name args) = LirFnCall name (map toLir args)
toLir (HirClosure arg body) = LirClosure arg (toLir body)
toLir (HirLet name expr body) = LirLet name (toLir expr) (toLir body)
toLir (HirMatch expr arms fallback) =
  LirMatch (toLir expr) (map (bimap toLir toLir) arms) (fmap toLir fallback)
toLir (HirDeref expr) = LirDeref (toLir expr)
toLir HirNoneInstance = LirNoneInstance

toLirType :: HirType -> LirType
toLirType (HirEnumType name generics) =
  LirEnumType name (map toLirType generics)
toLirType (HirGeneric name) = LirGeneric name
toLirType (HirBruijn _) = __IMPOSSIBLE__
toLirType (HirFn arg_type ret_type) =
  LirFn (toLirType arg_type) (toLirType ret_type)
toLirType HirNone = LirNone

unique :: Eq a => [a] -> [a]
unique []     = []
unique (x:xs) = x : unique (filter (x /=) xs)

extractGenericsFromType :: LirType -> [LirType]
extractGenericsFromType (LirEnumType _ gs) = gs
extractGenericsFromType x@(LirGeneric _) = [x]
extractGenericsFromType (LirFn arg ret) =
  extractGenericsFromType arg ++ extractGenericsFromType ret
extractGenericsFromType LirNone = []

toLirStmts :: HirStmt -> [LirStmt]
toLirStmts (HirFunction name arg_types body) =
  [LirFunction name (map toLirType arg_types) (toLir body)]
toLirStmts (HirConstructor name variants) = do
  let lirVariants = map (second (map toLirType)) variants
  let generics =
        unique $
        concatMap (\(_, x) -> concatMap extractGenericsFromType x) lirVariants
  let enumConstructor = LirEnum name generics lirVariants
  [enumConstructor]
