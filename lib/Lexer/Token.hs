module Lexer.Token where

import Protolude

data Token = Illegal
           | EOF
           | Ident Text
           | IntLiteral Int
           | Assign
           | Plus
           | Comma
           | SemiColon
           | LParen
           | RParen
           | LBrace
           | RBrace
           | Function
           | Let
           deriving (Show, Eq)
