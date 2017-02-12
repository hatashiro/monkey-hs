module Parser.Types where

import Protolude

import Common.ParserT
import Lexer.Token

newtype ParserError = ParserError Text
                    deriving (Show, Typeable)

instance Exception ParserError

type Parser = ParserT [Token] ParserError Identity

execParser :: Parser a -> [Token] -> a
execParser = (runIdentity .) . execParserT
