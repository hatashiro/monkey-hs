module Parser.AST where

import Protolude

import Lexer.Token

newtype Program = Program [Stmt]
                deriving (Show, Eq)

data Stmt = LetStatement Ident Expr
          deriving (Show, Eq)

data Expr = IdentExpr Ident
          deriving (Show, Eq)

newtype Ident = Ident Text
              deriving (Show, Eq)

newtype Node = Node Token -- empty dummy node
             deriving (Show, Eq)
