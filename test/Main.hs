module Main where

import Protolude

import Test.Hspec

import qualified ParserTSpec as ParserT
import qualified LexerSpec as Lexer
import qualified ParserSpec as Parser

main :: IO ()
main = hspec $ ParserT.spec
            >> Lexer.spec
            >> Parser.spec
