module Agda.Compiler.Lir where

import           Data.List (intercalate)
import           Data.Text (Text)
import qualified Data.Text as T

type LirIdent = Text

type LirArm = (LirExpr, LirExpr)

data LirExpr
  = LirVarRef LirIdent
    -- ^ variable reference: identifier
  | LirEnumConstructor LirIdent LirIdent [LirExpr]
    -- ^ enum constructor: enum name, variant name, args
  | LirFnCall LirIdent [LirExpr]
    -- ^ function call: name, args
  | LirClosureCall LirIdent [LirExpr]
    -- ^ closure call: name, args
  | LirClosure LirIdent LirExpr
    -- ^ single argument closure: argument name, body
  | LirLet LirIdent LirExpr LirExpr
    -- ^ single let expression: name, value, body
  | LirMatch LirExpr [LirArm] (Maybe LirExpr)
    -- ^ match statement: expression, arms, fallback
  | LirDeref LirExpr
    -- ^ dereference: expression
  | LirBox LirExpr
    -- ^ box shorthand: expression
  | LirNoneInstance
    -- ^ none instance
  | LirWildcard
    -- ^ match wildcard
  | LirUnreachable
    -- ^ match unreachable
  deriving (Eq)

instance Show LirExpr where
  show (LirVarRef name) = T.unpack name
  show (LirEnumConstructor datatype constructor args) =
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
    -- ^ type with name: name, generics
  | LirGeneric LirIdent
    -- ^ generic type parameter: name
  | LirFn LirType LirType
    -- ^ function type: argument type, return type
  | LirFnOnce LirType LirType
    -- ^ once function type: argument type, return type
  | LirBoxed LirType
    -- ^ boxed type: inner type
  | LirNone
    -- ^ none type
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
    -- ^ function: name, args, return type, body
  | LirEnum LirIdent [LirType] [(LirIdent, [LirType])]
    -- ^ enum: name, generics, variants
  | LirTypeAlias LirIdent LirType [LirType]
    -- ^ type alias: name, type, generics

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
