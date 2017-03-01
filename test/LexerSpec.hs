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

code2 :: Text
code2 = [r|
if (a == 10) {
  return a;
} else if (a != 20) {
  return !a;
} else if (a > 20) {
  return -30 / 40 * 50;
} else if (a < 30) {
  return true;
}
return false;
|]

spec :: Spec
spec = do
  describe "lexer" $ do
    it "special chars" $ do
      lex "=+(){},;" `shouldBe` Right [ Assign
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
      lex code `shouldBe` Right [ Let
                                , Ident "five"
                                , Assign
                                , IntLiteral 5
                                , SemiColon
                                , Let
                                , Ident "ten"
                                , Assign
                                , IntLiteral 10
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

    it "complex code 2" $ do
      lex code2 `shouldBe` Right [ If
                                 , LParen
                                 , Ident "a"
                                 , Eq
                                 , IntLiteral 10
                                 , RParen
                                 , LBrace
                                 , Return
                                 , Ident "a"
                                 , SemiColon
                                 , RBrace
                                 , Else
                                 , If
                                 , LParen
                                 , Ident "a"
                                 , NotEq
                                 , IntLiteral 20
                                 , RParen
                                 , LBrace
                                 , Return
                                 , Not
                                 , Ident "a"
                                 , SemiColon
                                 , RBrace
                                 , Else
                                 , If
                                 , LParen
                                 , Ident "a"
                                 , GreaterThan
                                 , IntLiteral 20
                                 , RParen
                                 , LBrace
                                 , Return
                                 , Minus
                                 , IntLiteral 30
                                 , Divide
                                 , IntLiteral 40
                                 , Multiply
                                 , IntLiteral 50
                                 , SemiColon
                                 , RBrace
                                 , Else
                                 , If
                                 , LParen
                                 , Ident "a"
                                 , LessThan
                                 , IntLiteral 30
                                 , RParen
                                 , LBrace
                                 , Return
                                 , BoolLiteral True
                                 , SemiColon
                                 , RBrace
                                 , Return
                                 , BoolLiteral False
                                 , SemiColon
                                 , EOF
                                 ]

    it "id with numbers" $ do
      lex "hello2 hel301oo120" `shouldBe` Right [ Ident "hello2"
                                                , Ident "hel301oo120"
                                                , EOF
                                                ]
