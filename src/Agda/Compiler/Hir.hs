module Agda.Compiler.Hir where

import           Data.List (intercalate)
import           Data.Text (Text)
import qualified Data.Text as T

type HirIdent = Text

type HirArm = (HirExpr, HirExpr)

data HirExpr
  = HirVarRef HirIdent
    -- datatype_name constructor_name args
  | HirDataConstructor HirIdent HirIdent [HirExpr]
    -- name arguments
  | HirFnCall HirIdent [HirExpr]
    -- arg_name body
  | HirClosure HirIdent HirExpr
    -- name expr body
  | HirLet HirIdent HirExpr HirExpr
    -- clause [match value] fallback
  | HirMatch HirExpr [HirArm] (Maybe HirExpr)
    -- expr
  | HirDeref HirExpr
  | HirNoneInstance

instance Show HirExpr where
  show (HirVarRef name) = T.unpack name
  show (HirDataConstructor datatype constructor args) =
    T.unpack datatype ++
    "::" ++
    T.unpack constructor ++ "(" ++ intercalate ", " (map show args) ++ ")"
  show (HirFnCall name args) =
    T.unpack name ++ "(" ++ intercalate ", " (map show args) ++ ")"
  show (HirClosure name body) = "|" ++ T.unpack name ++ "| " ++ show body
  show (HirLet name expr body) =
    "let " ++ T.unpack name ++ " = " ++ show expr ++ "\n" ++ show body
  show (HirMatch clause arms Nothing) =
    "match " ++ show clause ++ "\n" ++ intercalate "\n" (map show arms)
  show (HirMatch clause arms (Just fallback)) =
    "match " ++
    show clause ++
    "\n" ++ intercalate "\n" (map show arms) ++ "\n" ++ show fallback
  show (HirDeref expr) = "*" ++ show expr
  show HirNoneInstance = "()"

data HirType
  = HirEnumType HirIdent [HirType]
  | HirGeneric HirIdent
  | HirBruijn Int
  | HirFn HirType HirType
  | HirNone

instance Show HirType where
  show (HirEnumType name []) = T.unpack name
  show (HirEnumType name generics) =
    T.unpack name ++ "<" ++ intercalate ", " (map show generics) ++ ">"
  show (HirGeneric name) = T.unpack name
  show (HirBruijn x) = "@" ++ show x
  show (HirFn arg_type return_type) =
    "(" ++ show arg_type ++ " -> " ++ show return_type ++ ")"
  show HirNone = "None"

data HirStmt
  = HirFunction HirIdent [HirType] HirExpr
  | HirConstructor HirIdent [(HirIdent, [HirType])]

instance Show HirStmt where
  show (HirFunction name args body) =
    "fn " ++
    T.unpack name ++
    "(" ++ intercalate ", " (map show args) ++ ")\n" ++ show body
  show (HirConstructor name fields) =
    "data " ++
    T.unpack name ++
    "(" ++
    intercalate
      ", "
      (map
         (\(a, b) -> T.unpack a ++ ": " ++ intercalate ", " (map show b))
         fields) ++
    ")"
