module ParserSpec where

import Protolude

import Lexer (lex)
import Parser (parse)
import Parser.AST

import Test.Hspec

synAna :: Text -> Program
synAna = parse . lex

spec :: Spec
spec = do
  describe "parser" $
    it "empty" $
      synAna "" `shouldBe` Program []
