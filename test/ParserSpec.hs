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

exFn1 :: Text
exFn1 = [r|
fn() {
  return foobar + barfoo;
}
|]

exFn2 :: Text
exFn2 = [r|
fn(x, y) {
  return x + y;
}
|]

exFn3 :: Text
exFn3 = [r|
fn() {
  return fn (x, y, z, zz) { return x > y; };
}
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

    it "prefix expr" $ do
      synAna "-(foobar);" `shouldBe` Program [ ExprStmt $ PrefixExpr PrefixMinus (IdentExpr (Ident "foobar")) ]
      synAna "(+(10))" `shouldBe` Program [ ExprStmt $ PrefixExpr PrefixPlus (LitExpr (IntLiteral 10)) ]
      synAna "(((!true)))" `shouldBe` Program [ ExprStmt $ PrefixExpr Not (LitExpr (BoolLiteral True)) ]

    it "infix expr" $ do
      synAna "10 + 20" `shouldBe` Program [ ExprStmt $
                                            InfixExpr
                                              Plus
                                              (LitExpr (IntLiteral 10))
                                              (LitExpr (IntLiteral 20))
                                          ]
      synAna "10 * 20" `shouldBe` Program [ ExprStmt $
                                            InfixExpr
                                              Multiply
                                              (LitExpr (IntLiteral 10))
                                              (LitExpr (IntLiteral 20))
                                          ]

      synAna "10 + 5 / -20 - (x + x)" `shouldBe` synAna "10 + (5 / (-20)) - (x + x)"
      synAna "10 + 5 / -20 - (x + x)" `shouldBe` Program [ ExprStmt $
                                            InfixExpr
                                              Minus
                                              (InfixExpr
                                                Plus
                                                (LitExpr (IntLiteral 10))
                                                (InfixExpr
                                                  Divide
                                                  (LitExpr (IntLiteral 5))
                                                  (PrefixExpr PrefixMinus (LitExpr (IntLiteral 20)))
                                                )
                                              )
                                              (InfixExpr
                                                Plus
                                                (IdentExpr (Ident "x"))
                                                (IdentExpr (Ident "x"))
                                              )
                                          ]

    it "op precedence" $ do
      let pTest x y = synAna x `shouldBe` synAna y
      "!-a" `pTest` "(!(-a))"
      "a + b + c" `pTest` "((a + b) + c)"
      "a + b - c" `pTest` "((a + b) - c)"
      "a * b * c" `pTest` "((a * b) * c)"
      "a * b / c" `pTest` "((a * b) / c)"
      "a + b / c" `pTest` "(a + (b / c))"
      "a + b * c + d / e - f" `pTest` "(((a + (b * c)) + (d / e)) - f)"
      "3 + 4; -5 * 5" `pTest` "(3 + 4)((-5) * 5)"
      "5 > 4 == 3 < 4" `pTest` "((5 > 4) == (3 < 4))"
      "5 < 4 != 3 > 4" `pTest` "((5 < 4) != (3 > 4))"
      "3 + 4 * 5 == 3 * 1 + 4 * 5" `pTest` "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))"

    it "if expr" $ do
      synAna "if (x < y) { x }" `shouldBe`
        Program
          [ ExprStmt $ IfExpr
              (InfixExpr LessThan (IdentExpr (Ident "x")) (IdentExpr (Ident "y")))
              [ ExprStmt (IdentExpr (Ident "x")) ]
              Nothing
          ]

      synAna "if (x < y) { x } else { y }" `shouldBe`
        Program
          [ ExprStmt $ IfExpr
              (InfixExpr LessThan (IdentExpr (Ident "x")) (IdentExpr (Ident "y")))
              [ ExprStmt (IdentExpr (Ident "x")) ]
              (Just [ ExprStmt (IdentExpr (Ident "y")) ])
          ]

    it "function expr" $ do
      synAna exFn1 `shouldBe` Program
        [ ExprStmt $ FnExpr []
          [ ReturnStmt $ InfixExpr Plus (IdentExpr (Ident "foobar")) (IdentExpr (Ident "barfoo"))
          ]
        ]
      synAna exFn2 `shouldBe` Program
        [ ExprStmt $ FnExpr [Ident "x", Ident "y"]
          [ ReturnStmt $ InfixExpr Plus (IdentExpr (Ident "x")) (IdentExpr (Ident "y"))
          ]
        ]
      synAna exFn3 `shouldBe` Program
        [ ExprStmt $ FnExpr []
          [ ReturnStmt $ FnExpr [Ident "x", Ident "y", Ident "z", Ident "zz"]
            [ ReturnStmt $ InfixExpr GreaterThan (IdentExpr (Ident "x")) (IdentExpr (Ident "y"))
            ]
          ]
        ]
