module Agda.Compiler.RustSyntax where

import Data.List (intercalate)

-- Based on: https://hackage.haskell.org/package/flp-0.1.0.0/docs/src/Language.Rust.Syntax.AST.html

newtype Ident = Ident String

instance Show Ident where
  show (Ident x) = x

newtype Variant = Variant Ident

instance Show Variant where
  show (Variant ident) = show ident ++ "()"

newtype Type = EnumType Ident

instance Show Type where
  show (EnumType ident) = show ident

data Argument = Argument Ident Type

instance Show Argument where
  show (Argument ident ty) = show ident ++ ": " ++ show ty

data FunctionDecl = FunctionDecl [Argument] (Maybe Type)

instance Show FunctionDecl where
  show (FunctionDecl args Nothing) =
    "(" ++ intercalate ", " (map show args)
      ++ ")"
  show (FunctionDecl args (Just returnType)) =
    "(" ++ intercalate ", " (map show args)
      ++ ") -> "
      ++ show returnType

newtype Expr = Expr String

instance Show Expr where
  show (Expr _) = "EXPR"

newtype Statement = Return Expr

instance Show Statement where
  show (Return expr) = "return " ++ show expr ++ ";"

newtype Block = Block [Statement]

instance Show Block where
  show (Block stmts) = "{\n\t" ++ intercalate "\n\t" (map show stmts) ++ "\n}"

data Item = Enum Ident [Variant] | Function Ident FunctionDecl Block

instance Show Item where
  show (Enum ident variants) =
    "enum " ++ show ident
      ++ " {\n\t"
      ++ intercalate ",\n\t" (map show variants)
      ++ "\n}"
  show (Function ident decl body) = "fn " ++ show ident ++ show decl ++ show body
