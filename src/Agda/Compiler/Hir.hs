module Agda.Compiler.Hir where

import           Data.List (intercalate)
import           Data.Text (Text)
import qualified Data.Text as T

type HirIdent = Text

type HirArm = (HirExpr, HirExpr)

data HirExpr
  = HirVarRef HirIdent
    -- ^ variable reference: identifier
  | HirDataConstructor HirIdent HirIdent [HirExpr]
    -- ^ construct data variant: data type name, variant name, args
  | HirFnCall HirIdent [HirExpr]
    -- ^ function call: name, args
  | HirClosureCall HirIdent [HirExpr]
    -- ^ closure call: name, args
  | HirClosure HirIdent HirExpr
    -- ^ single argument closure: argument name, body
  | HirClone HirExpr
  | HirLet HirIdent HirExpr HirExpr
    -- ^ let expression: name, value, body
  | HirMatch HirExpr [HirArm] (Maybe HirExpr)
    -- ^ match expression: expression, arms, fallback
  | HirDeref HirExpr
    -- ^ dereference: expression
  | HirNoneInstance
    -- ^ none instance

instance Show HirExpr where
  show (HirVarRef name) = T.unpack name
  show (HirDataConstructor datatype constructor args) =
    T.unpack datatype ++
    "::" ++
    T.unpack constructor ++ "(" ++ intercalate ", " (map show args) ++ ")"
  show (HirFnCall name args) =
    T.unpack name ++ "(" ++ intercalate ", " (map show args) ++ ")"
  show (HirClosureCall name args) =
    "(" ++ T.unpack name ++ ")(" ++ intercalate ", " (map show args) ++ ")"
  show (HirClosure name body) = "|" ++ T.unpack name ++ "| " ++ show body
  show (HirClone expr) = "clone " ++ show expr
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
  = HirNamedType HirIdent [HirType]
    -- ^ type with name: name, generics
  | HirGeneric HirIdent
    -- ^ generic type parameter: name
  | HirBruijn Int
    -- ^ de bruijn indexed variable: index (should be completely eliminated
    -- before converting to LIR)
  | HirFn HirType HirType
    -- ^ function type: argument type, return type
  | HirNone
    -- ^ none type

instance Show HirType where
  show (HirNamedType name []) = T.unpack name
  show (HirNamedType name generics) =
    T.unpack name ++ "<" ++ intercalate ", " (map show generics) ++ ">"
  show (HirGeneric name) = T.unpack name
  show (HirBruijn x) = "@" ++ show x
  show (HirFn arg_type return_type) =
    "(" ++ show arg_type ++ " -> " ++ show return_type ++ ")"
  show HirNone = "None"

data HirStmt
  = HirFunction HirIdent [HirType] HirExpr
    -- ^ function: name, args, body
  | HirConstructor HirIdent [(HirIdent, [HirType])]
    -- ^ datta constructor: name, variants

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
