module ParserSpec where

import Protolude

import Lexer (lex)
import Parser (parse)
import Parser.AST
import Text.RawString.QQ

import Test.Hspec

exLetStmts :: Text
exLetStmts = [r|
let x = 5;
let y = 10;
let foobar = 838383;
let boo = true;
|]

exReturnStmts :: Text
exReturnStmts = [r|
return 5;
return 10;
return 838383;
return true;
|]

exSomeStmts :: Text
exSomeStmts = [r|
let x = 5;
return 10;
15;
let y = 20;
return false;
|]

synAna :: Text -> Program
synAna = parse . lex

spec :: Spec
spec = do
  describe "parser" $ do
    it "empty" $
      synAna "" `shouldBe` Program []

    it "let statements" $
      synAna exLetStmts `shouldBe` Program [ LetStmt (Ident "x") (LitExpr (IntLiteral 5))
                                           , LetStmt (Ident "y") (LitExpr (IntLiteral 10))
                                           , LetStmt (Ident "foobar") (LitExpr (IntLiteral 838383))
                                           , LetStmt (Ident "boo") (LitExpr (BoolLiteral True))
                                           ]

    it "return statements" $
      synAna exReturnStmts `shouldBe` Program [ ReturnStmt (LitExpr (IntLiteral 5))
                                              , ReturnStmt (LitExpr (IntLiteral 10))
                                              , ReturnStmt (LitExpr (IntLiteral 838383))
                                              , ReturnStmt (LitExpr (BoolLiteral True))
                                              ]

    it "some statements" $
      synAna exSomeStmts `shouldBe` Program [ LetStmt (Ident "x") (LitExpr (IntLiteral 5))
                                            , ReturnStmt (LitExpr (IntLiteral 10))
                                            , ExprStmt (LitExpr (IntLiteral 15))
                                            , LetStmt (Ident "y") (LitExpr (IntLiteral 20))
                                            , ReturnStmt (LitExpr (BoolLiteral False))
                                            ]

    it "identifier" $ do
      synAna "foobar;" `shouldBe` Program [ ExprStmt (IdentExpr (Ident "foobar")) ]
      synAna "foobar" `shouldBe` Program [ ExprStmt (IdentExpr (Ident "foobar")) ]

    it "prefix expr" $ do
      synAna "-foobar;" `shouldBe` Program [ ExprStmt $ PrefixExpr PrefixMinus (IdentExpr (Ident "foobar")) ]
      synAna "+10" `shouldBe` Program [ ExprStmt $ PrefixExpr PrefixPlus (LitExpr (IntLiteral 10)) ]
      synAna "!true" `shouldBe` Program [ ExprStmt $ PrefixExpr Not (LitExpr (BoolLiteral True)) ]
