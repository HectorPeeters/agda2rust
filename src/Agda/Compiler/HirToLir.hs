module Agda.Compiler.HirToLir where

import           Agda.Compiler.Hir
import           Agda.Compiler.Lir

toLir :: HirExpr -> LirExpr
toLir (HirVarRef x) = LirVarRef x
toLir (HirDataConstructor datatype constructor args) =
  LirDataConstructor datatype constructor (map toLir args)
toLir (HirFnCall name args) = LirFnCall name (map toLir args)
toLir (HirClosure arg body) = LirClosure arg (toLir body)
toLir (HirLet name expr body) = LirLet name (toLir expr) (toLir body)
toLir (HirMatch expr arms fallback) =
  LirMatch
    (toLir expr)
    (map (\(a, b) -> (toLir a, toLir b)) arms)
    (fmap toLir fallback)
toLir (HirDeref expr) = LirDeref (toLir expr)
toLir HirNoneInstance = LirNoneInstance

toLirStmts :: [HirStmt] -> [LirStmt]
toLirStmts _ = error "AAA"