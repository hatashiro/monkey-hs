module Lexer.Token where

import Protolude

data Token = Illegal
           | EOF
           -- identifier and literals
           | Ident Text
           | IntLiteral Integer
           | BoolLiteral Bool
           -- statements
           | Assign
           | If
           | Else
           -- operators
           | Plus
           | Minus
           | Divide
           | Multiply
           | Eq
           | NotEq
           | GreaterThan
           | LessThan
           | Not
           -- reserved words
           | Function
           | Let
           | Return
           -- punctuations
           | Comma
           | SemiColon
           | LParen
           | RParen
           | LBrace
           | RBrace
           deriving (Show, Eq)
