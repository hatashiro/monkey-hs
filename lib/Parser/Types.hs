module Parser.Types where

import Protolude

import Common.ParserT
import Lexer.Token

type Parser = ParserT [Token] Identity

execParser :: Parser a -> [Token] -> a
execParser = (runIdentity .) . execParserT
