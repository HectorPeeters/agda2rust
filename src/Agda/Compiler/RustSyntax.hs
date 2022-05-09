module Agda.Compiler.RustSyntax where

import Data.List (intercalate)

-- Based on: https://hackage.haskell.org/package/flp-0.1.0.0/docs/src/Language.Rust.Syntax.AST.html

newtype RsIdent = RsIdent String deriving (Eq, Ord)

instance Show RsIdent where
  show (RsIdent x) = x

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

data RsExpr = RsReturn (Maybe RsExpr)

instance Show RsExpr where
  show (RsReturn Nothing) = "return"
  show (RsReturn (Just expr)) = "return " ++ show expr

newtype RsStatement = RsSemi RsExpr

instance Show RsStatement where
  show (RsSemi expr) = show expr ++ ";"

newtype RsBlock = RsBlock [RsStatement]

instance Show RsBlock where
  show (RsBlock stmts) = "{\n\t" ++ intercalate "\n\t" (map show stmts) ++ "\n}"

data RsItem = RsEnum RsIdent [RsVariant] | RsFunction RsIdent RsFunctionDecl RsBlock

instance Show RsItem where
  show (RsEnum ident variants) =
    "use " ++ show ident ++ "::*;\nenum " ++ show ident
      ++ " {\n\t"
      ++ intercalate ",\n\t" (map show variants)
      ++ "\n}"
  show (RsFunction ident decl body) = "fn " ++ show ident ++ show decl ++ " " ++ show body
