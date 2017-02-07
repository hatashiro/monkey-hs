module LexerSpec where

import Protolude

import Test.Hspec

import Lexer
import Lexer.Token
import Text.RawString.QQ

code :: Text
code = [r|
let five = 5;
let ten = 10;
let add = fn(x, y) {
  x + y;
};
let result = add(five, ten);
|]

spec :: IO ()
spec = hspec $
  describe "lexer" $ do
    it "special chars" $ do
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

    it "complex code" $ do
      lex code `shouldBe` [ Let
                          , Ident "five"
                          , Assign
                          , IntLiteral "5"
                          , SemiColon
                          , Let
                          , Ident "ten"
                          , Assign
                          , IntLiteral "10"
                          , SemiColon
                          , Let
                          , Ident "add"
                          , Assign
                          , Function
                          , LParen
                          , Ident "x"
                          , Comma
                          , Ident "y"
                          , RParen
                          , LBrace
                          , Ident "x"
                          , Plus
                          , Ident "y"
                          , SemiColon
                          , RBrace
                          , SemiColon
                          , Let
                          , Ident "result"
                          , Assign
                          , Ident "add"
                          , LParen
                          , Ident "five"
                          , Comma
                          , Ident "ten"
                          , RParen
                          , SemiColon
                          , EOF
                          ]
