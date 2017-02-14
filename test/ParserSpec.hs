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
