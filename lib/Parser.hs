module Parser where

import Protolude hiding (one, many)

import Common.ParserT
import Lexer.Token
import Parser.AST
import Parser.Types
import Utils ((<<))

parseProgram :: Parser Program
parseProgram = do
  return $ Program [] -- FIXME

parse :: [Token] -> Program
parse = execParser (parseProgram << one (== EOF))
