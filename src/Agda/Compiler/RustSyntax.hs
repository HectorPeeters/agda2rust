module Agda.Compiler.RustSyntax where

import Data.List (intercalate)
import Data.Text (Text)
import qualified Data.Text as T

-- Based on: https://hackage.haskell.org/package/flp-0.1.0.0/docs/src/Language.Rust.Syntax.AST.html

newtype RsIdent = RsIdent Text deriving (Eq, Ord)

instance Show RsIdent where
  show (RsIdent x) = T.unpack x

newtype RsVariant = RsVariant RsIdent

instance Show RsVariant where
  show (RsVariant ident) = show ident ++ "()"

newtype RsType = RsEnumType RsIdent

instance Show RsType where
  show (RsEnumType ident) = show ident

data RsArgument = RsArgument RsIdent RsType

instance Show RsArgument where
  show (RsArgument ident ty) = show ident ++ ": " ++ show ty

data RsFunctionDecl = RsFunctionDecl [RsArgument] (Maybe RsType)

instance Show RsFunctionDecl where
  show (RsFunctionDecl args Nothing) =
    "(" ++ intercalate ", " (map show args)
      ++ ")"
  show (RsFunctionDecl args (Just returnRsType)) =
    "(" ++ intercalate ", " (map show args)
      ++ ") -> "
      ++ show returnRsType

data RsField = RsField RsIdent RsExpr

instance Show RsField where
  show (RsField ident expr) = show ident ++ ": " ++ show expr

-- TODO: This only supports pattern matching with identifiers
data RsArm = RsArm RsExpr RsExpr

instance Show RsArm where
  show (RsArm ident expr) = show ident ++ " => " ++ show expr ++ ","

-- TODO: add capture data to RsClosure
data RsExpr
  = RsReturn (Maybe RsExpr)
  | RsClosure [RsIdent] RsExpr
  | RsMatch RsExpr [RsArm] (Maybe RsExpr)
  | -- NOTE: this should techinically be a Path but lets try and avoid those for now
    RsVarRef RsIdent
  | RsDataConstructor RsIdent [RsExpr]

instance Show RsExpr where
  show (RsReturn Nothing) = "return"
  show (RsReturn (Just expr)) = "return " ++ show expr
  show (RsClosure args expr) = "|" ++ intercalate ", " (map show args) ++ "| {" ++ show expr ++ "}"
  show (RsMatch expr arms Nothing) = "match " ++ show expr ++ " {\n" ++ intercalate "\n" (map show arms) ++ "\n}"
  show (RsMatch expr arms (Just fallback)) = "match " ++ show expr ++ " {\n" ++ intercalate "\n" (map show arms) ++ "\n_ =>" ++ show fallback ++ "\n}"
  show (RsVarRef ident) = show ident
  show (RsDataConstructor name args) = show name ++ "(" ++ intercalate ", " (map show args) ++ ")"

data RsStatement = RsSemi RsExpr | RsNoSemi RsExpr

instance Show RsStatement where
  show (RsSemi expr) = show expr ++ ";"
  show (RsNoSemi expr) = show expr

newtype RsBlock = RsBlock [RsStatement]

instance Show RsBlock where
  show (RsBlock stmts) = "{\n\t" ++ intercalate "\n\t" (map show stmts) ++ "\n}"

data RsItem = RsEnum RsIdent [RsVariant] | RsFunction RsIdent RsFunctionDecl RsBlock

instance Show RsItem where
  show (RsEnum ident variants) =
    "use " ++ show ident ++ "::*;\n\n#[derive(Debug)]\nenum " ++ show ident
      ++ " {\n\t"
      ++ intercalate ",\n\t" (map show variants)
      ++ "\n}"
  show (RsFunction ident decl body) = "fn " ++ show ident ++ show decl ++ " " ++ show body
