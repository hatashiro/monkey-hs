module Parser.AST where

import Protolude

import Lexer.Token

newtype Program = Program [Stmt]
                deriving (Show, Eq)

data Stmt = LetStmt Ident Expr
          | ReturnStmt Expr
          | ExprStmt Expr
          deriving (Show, Eq)

data Expr = IdentExpr Ident
          | LitExpr Literal
          | PrefixExpr Prefix Expr
          deriving (Show, Eq)

data Literal = IntLiteral Integer
             | BoolLiteral Bool
             deriving (Show, Eq)

newtype Ident = Ident Text
              deriving (Show, Eq)

data Prefix = PrefixPlus | PrefixMinus | Not
            deriving (Show, Eq)

newtype Node = Node Token -- empty dummy node
             deriving (Show, Eq)
