module Agda.Compiler.RustSyntax where

import Data.List (intercalate)
import Data.Sequence (mapWithIndex)
import Data.Text (Text)
import qualified Data.Text as T

-- Based on: https://hackage.haskell.org/package/flp-0.1.0.0/docs/src/Language.Rust.Syntax.AST.html
rustPrelude = "#![feature(type_alias_impl_trait)]\n\n"

newtype RsIdent
  = RsIdent Text
  deriving (Eq, Ord)

instance Show RsIdent where
  show (RsIdent x) = T.unpack x

data RsVariant
  = RsVariant RsIdent [RsType]

instance Show RsVariant where
  show (RsVariant ident types) =
    show ident ++ "(" ++ intercalate ", " (map show types) ++ ")"

data RsType
  = RsEnumType RsIdent [RsType]
  | RsBoxed RsType
  | RsFn RsType RsType
  | RsNone
  deriving (Eq)

instance Show RsType where
  show (RsEnumType ident []) = show ident
  show (RsEnumType ident generics) =
    show ident ++ "<" ++ intercalate ", " (map show generics) ++ ">"
  show (RsBoxed t) = "Box<" ++ show t ++ ">"
  show (RsFn a r) = "impl FnOnce(" ++ show a ++ ") -> " ++ show r
  show RsNone = "()"

data RsField
  = RsField RsIdent RsExpr

instance Show RsField where
  show (RsField ident expr) = show ident ++ ": " ++ show expr

-- TODO: This only supports pattern matching with identifiers
data RsArm
  = RsArm RsExpr RsExpr

instance Show RsArm where
  show (RsArm ident expr) = show ident ++ " => " ++ show expr ++ ","

-- TODO: add capture data to RsClosure
data RsExpr
  = RsReturn (Maybe RsExpr)
  | RsClosure [RsIdent] RsExpr
  | RsMatch RsExpr [RsArm] (Maybe RsExpr)
  | -- NOTE: this should techinically be a Path but lets try and avoid those for now
    RsVarRef RsIdent
  | RsDataConstructor RsIdent RsIdent [RsExpr]
  | RsFunctionCall RsIdent [RsExpr]
  | RsBox RsExpr
  | RsDeref RsExpr
  | RsLet RsIdent RsExpr RsExpr
  | RsNoneInstance
  | RsIntLit Integer
  | RsBinop String RsExpr RsExpr
  | RsIfElse RsExpr RsExpr RsExpr

instance Show RsExpr where
  show (RsReturn Nothing) = "return"
  show (RsReturn (Just expr)) = "return " ++ show expr
  show (RsClosure args expr) =
    "move |" ++ intercalate ", " (map show args) ++ "| {" ++ show expr ++ "}"
  show (RsMatch expr arms Nothing) =
    "match "
      ++ show expr
      ++ " {\n"
      ++ intercalate "\n" (map show arms)
      ++ "\n_ => unreachable!(),\n}"
  show (RsMatch expr arms (Just fallback)) =
    "match "
      ++ show expr
      ++ " {\n"
      ++ intercalate "\n" (map show arms)
      ++ "\n_ =>"
      ++ show fallback
      ++ "\n}"
  show (RsVarRef ident) = show ident
  show (RsDataConstructor name variantName args) =
    show name
      ++ "::"
      ++ show variantName
      ++ "("
      ++ intercalate ", " (map show args)
      ++ ")"
  show (RsFunctionCall name args) =
    show name ++ "()" ++ intercalate "" (map (\x -> "(" ++ show x ++ ")") args)
  show (RsBox expr) = "Box::new(" ++ show expr ++ ")"
  show (RsDeref expr) = "*" ++ show expr
  show (RsLet ident expr body) = "{let " ++ show ident ++ " = " ++ show expr ++ ";\n" ++ show body ++ "}"
  show RsNoneInstance = "()"
  show (RsIntLit x) = show x
  show (RsBinop op left right) = show left ++ " " ++ op ++ " " ++ show right
  show (RsIfElse cond a b) = "if " ++ show cond ++ " {\n" ++ show a ++ "\n} else {\n" ++ show b ++ "\n}"

data RsStatement
  = RsSemi RsExpr
  | RsNoSemi RsExpr

instance Show RsStatement where
  show (RsSemi expr) = show expr ++ ";"
  show (RsNoSemi expr) = show expr

newtype RsBlock
  = RsBlock [RsStatement]

instance Show RsBlock where
  show (RsBlock stmts) = "{\n\t" ++ intercalate "\n\t" (map show stmts) ++ "\n}"

data RsItem
  = RsEnum RsIdent [RsType] [RsVariant]
  | RsFunction RsIdent [RsType] (Maybe RsType) RsBlock

unique :: Eq a => [a] -> [a]
unique [] = []
unique (x : xs) = x : unique (filter (x /=) xs)

formatGenericArgs :: [RsType] -> String
formatGenericArgs [] = ""
formatGenericArgs xs = "<" ++ intercalate ", " (map show xs) ++ ">"

instance Show RsItem where
  show (RsEnum ident generics variants) =
    "#[derive(Debug)]\nenum "
      ++ show ident
      ++ formatGenericArgs generics
      ++ " {\n\t"
      ++ intercalate ",\n\t" (map show variants)
      ++ "\n}"
  show (RsFunction ident [] (Just ret) body) = do
    "fn " ++ show ident ++ "() -> " ++ show ret ++ show body
  show (RsFunction ident as (Just ret) body) =
    -- we are generiting the curry type definitions in reverse order
    do
      let args = reverse as
      -- TODO: we can't just assume that all single letter types are generics
      -- lets create a list of all the generic arguments in the current function
      -- this is done by filtering all the unique arguments and checking if their length is 1
      let genericArgs =
            formatGenericArgs $
              unique
                ( filter
                    (\x -> length (show x) == 1)
                    args
                    ++ [ret | length (show ret) == 1]
                )
      -- lets create the last curry type which returns the final value of the function
      let firstCurryType =
            "type "
              ++ show ident
              ++ "0"
              ++ genericArgs
              ++ " = impl FnOnce("
              ++ show (head args)
              ++ ") -> "
              ++ show ret
              ++ ";\n"
      -- lets create all the intermediate curry types
      let restCurryLines =
            zipWith
              ( \i a ->
                  "type "
                    ++ show ident
                    ++ show i
                    ++ genericArgs
                    ++ " = impl FnOnce("
                    ++ show a
                    ++ ") -> "
                    ++ show ident
                    ++ show (i - 1)
                    ++ genericArgs
                    ++ ";"
              )
              [1 ..]
              (tail args)
      -- combine all the curry types into a string
      let curryTypes = firstCurryType ++ intercalate "\n" restCurryLines
      curryTypes
        ++ "\nfn "
        ++ show ident
        ++ genericArgs
        ++ "() -> "
        ++ show ident
        ++ show (length args - 1)
        ++ genericArgs
        ++ show body
