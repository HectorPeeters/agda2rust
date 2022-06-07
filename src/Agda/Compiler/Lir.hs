module Agda.Compiler.Lir where

import           Data.List (intercalate)
import           Data.Text (Text)
import qualified Data.Text as T

type LirIdent = Text

type LirArm = (LirExpr, LirExpr)

data LirExpr
  = LirVarRef LirIdent
    -- datatype_name constructor_name args
  | LirDataConstructor LirIdent LirIdent [LirExpr]
    -- name arguments
  | LirFnCall LirIdent [LirExpr]
    -- name arguments
  | LirClosureCall LirIdent [LirExpr]
    -- arg_name body
  | LirClosure LirIdent LirExpr
    -- name expr body
  | LirLet LirIdent LirExpr LirExpr
    -- clause [match value] fallback
  | LirMatch LirExpr [LirArm] (Maybe LirExpr)
    -- expr
  | LirDeref LirExpr
  | LirBox LirExpr
  | LirNoneInstance
  | LirWildcard
  | LirUnreachable
  deriving (Eq)

instance Show LirExpr where
  show (LirVarRef name) = T.unpack name
  show (LirDataConstructor datatype constructor args) =
    T.unpack datatype ++
    "::" ++
    T.unpack constructor ++ "(" ++ intercalate ", " (map show args) ++ ")"
  show (LirFnCall name args) =
    T.unpack name ++
    "()" ++ intercalate "" (map (\x -> "(" ++ show x ++ ")") args)
  show (LirClosureCall name args) =
    "(" ++
    T.unpack name ++
    ")" ++ intercalate "" (map (\x -> "(" ++ show x ++ ")") args)
  show (LirClosure name body) = "move |" ++ T.unpack name ++ "| " ++ show body
  show (LirLet name expr body) =
    "{let " ++
    T.unpack name ++ " = " ++ show expr ++ ";\n" ++ show body ++ "\n}"
  show (LirMatch clause arms Nothing) =
    "match " ++
    show clause ++
    "{\n" ++
    intercalate "\n" (map (\(l, r) -> show l ++ " => " ++ show r ++ ",") arms) ++
    "\n}"
  show (LirMatch clause arms (Just fallback)) =
    "match " ++
    show clause ++
    "{\n" ++
    intercalate "\n" (map show arms) ++ "\n_ => " ++ show fallback ++ "\n}"
  show (LirDeref expr) = "*" ++ show expr
  show (LirBox expr) = "Box::new(" ++ show expr ++ ")"
  show LirNoneInstance = "()"
  show LirWildcard = "_"
  show LirUnreachable = "unreachable!()"

formatGenerics :: [LirType] -> String
formatGenerics [] = ""
formatGenerics xs = "<" ++ intercalate ", " (map show xs) ++ ">"

data LirType
  = LirNamedType LirIdent [LirType]
  | LirGeneric LirIdent
  | LirFnOnce LirType LirType
  | LirFn LirType LirType
  | LirBoxed LirType
  | LirNone
  deriving (Eq)

instance Show LirType where
  show (LirNamedType name generics) = T.unpack name ++ formatGenerics generics
  show (LirGeneric name) = T.unpack name
  show (LirFnOnce arg_type return_type) =
    "impl FnOnce(" ++ show arg_type ++ ") -> " ++ show return_type
  show (LirFn arg_type return_type) =
    "impl Fn(" ++ show arg_type ++ ") -> " ++ show return_type
  show (LirBoxed t) = "Box<" ++ show t ++ ">"
  show LirNone = "()"

data LirStmt
  = LirFunction LirIdent [LirType] LirType LirExpr
  | LirEnum LirIdent [LirType] [(LirIdent, [LirType])]
  | LirTypeAlias LirIdent LirType [LirType]
  deriving (Eq)

instance Show LirStmt where
  show (LirFunction name generics ret_type body) =
    "fn " ++
    T.unpack name ++
    formatGenerics generics ++
    "() -> " ++ show ret_type ++ " {\n" ++ show body ++ "\n}"
  show (LirEnum name generics fields) =
    "enum " ++
    T.unpack name ++
    formatGenerics generics ++
    "{\n" ++
    intercalate
      "\n"
      (map
         (\(a, b) ->
            "\t" ++ T.unpack a ++ "(" ++ intercalate ", " (map show b) ++ "),")
         fields) ++
    "\n}"
  show (LirTypeAlias name typ generics) =
    "type " ++
    T.unpack name ++ formatGenerics generics ++ " = " ++ show typ ++ ";"
