module Agda.Compiler.Lir where

import           Data.List (intercalate)
import           Data.Text (Text)
import qualified Data.Text as T

type LirIdent = Text

type LirGeneric = LirIdent

type LirArm = (LirExpr, LirExpr)

data LirExpr
  = LirVarRef LirIdent
            -- datatype_name constructor_name args
  | LirDataConstructor LirIdent LirIdent [LirExpr]
            -- name arguments
  | LirFnCall LirIdent [LirExpr]
            -- arg_name body
  | LirClosure LirIdent LirExpr
            -- name expr body
  | LirLet LirIdent LirExpr LirExpr
            -- clause [match value] fallback
  | LirMatch LirExpr [LirArm] (Maybe LirExpr)
            -- expr
  | LirDeref LirExpr
  | LirNoneInstance

instance Show LirExpr where
  show (LirVarRef name) = T.unpack name
  show (LirDataConstructor datatype constructor args) =
    T.unpack datatype ++
    "::" ++
    T.unpack constructor ++ "(" ++ (intercalate ", " $ map show args) ++ ")"
  show (LirFnCall name args) =
    T.unpack name ++ "()" ++ (intercalate "" $ map (\x -> "(" ++ show x ++ ")") args)
  show (LirClosure name body) = "move |" ++ T.unpack name ++ "| " ++ show body
  show (LirLet name expr body) =
    "{let " ++ T.unpack name ++ " = " ++ show expr ++ ";\n" ++ show body ++ "\n}"
  show (LirMatch clause arms Nothing) =
    "match " ++ show clause ++ "{\n" ++ (intercalate "\n" $ map show arms) ++ "\n}"
  show (LirMatch clause arms (Just fallback)) =
    "match " ++
    show clause ++
    "{\n" ++ (intercalate "\n" $ map show arms) ++ "\n_ => " ++ show fallback ++ "\n}"
  show (LirDeref expr) = "*" ++ show expr
  show LirNoneInstance = "()"

data LirType
  = LirEnumType LirIdent [LirGeneric]
  | LirGeneric LirIdent
  | LirFn LirType LirType
  | LirNone

instance Show LirType where
  show (LirEnumType name []) = T.unpack name
  show (LirEnumType name generics) =
    T.unpack name ++ "<" ++ (intercalate ", " (map T.unpack generics)) ++ ">"
  show (LirGeneric name) = T.unpack name
  show (LirFn arg_type return_type) =
    "(" ++ show arg_type ++ " -> " ++ show return_type ++ ")"
  show LirNone = "None"

data LirStmt
  = LirFunction LirIdent [LirType] LirExpr
  | LirConstructor LirIdent [(LirIdent, [LirType])]

instance Show LirStmt where
  show (LirFunction name args body) =
    "fn " ++
    T.unpack name ++
    "(" ++ (intercalate ", " $ map show args) ++ ")\n" ++ show body
  show (LirConstructor name fields) =
    "data " ++
    T.unpack name ++
    "(" ++
    (intercalate
       ", "
       (map
          (\(a, b) -> T.unpack a ++ ": " ++ (intercalate ", " $ map show b))
          fields)) ++
    ")"
