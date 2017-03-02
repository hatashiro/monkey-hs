module Main where

import Protolude

import Test.Hspec

import qualified ParserTSpec as ParserT
import qualified LexerSpec as Lexer
import qualified ParserSpec as Parser
import qualified EvaluatorSpec as Evaluator

main :: IO ()
main = hspec $ ParserT.spec
            >> Lexer.spec
            >> Parser.spec
            >> Evaluator.spec
