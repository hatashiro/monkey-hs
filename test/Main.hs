module Main where

import Protolude

import Test.Hspec

import qualified LexerSpec as Lexer
import qualified ParserSpec as Parser

main :: IO ()
main = hspec $ Lexer.spec
            >> Parser.spec
