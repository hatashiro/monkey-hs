{-# LANGUAGE MultiParamTypeClasses #-}
module Lexer.Types where

import Protolude

import qualified Data.Text as T
import           Common.ParserT
import           Common.Stream

instance Stream Text Char where
  read = T.uncons

type Lexer = ParserT Text Identity

execLexer :: Lexer a -> Text -> Either ParserError a
execLexer = (runIdentity .) . execParserT
