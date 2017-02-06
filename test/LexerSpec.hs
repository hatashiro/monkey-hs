module LexerSpec where

import Protolude

import Test.Hspec

import Lexer
import Lexer.Token

spec :: IO ()
spec = hspec $
  describe "lexer" $ do
    it "test" $ do
      lex "=+(){},;" `shouldBe` [ Assign
                                , Plus
                                , LParen
                                , RParen
                                , LBrace
                                , RBrace
                                , Comma
                                , SemiColon
                                , EOF
                                ]
