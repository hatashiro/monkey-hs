{-# LANGUAGE MultiParamTypeClasses #-}
module Lexer.Types where

import Protolude

import qualified Data.Text as T
import           Common.ParserT
import           Common.Stream

instance Stream Text Char where
  read = T.uncons

newtype LexerError = LexerError Text
                   deriving (Show, Typeable)

instance Exception LexerError

type Lexer = ParserT Text LexerError Identity

execLexer :: Lexer a -> Text -> a
execLexer = (runIdentity .) . execParserT
