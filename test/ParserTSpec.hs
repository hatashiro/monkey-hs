module ParserTSpec where

import Protolude

import Common.ParserT

import Test.Hspec

type Parser m = ParserT [Integer] m

spec :: Spec
spec = describe "ParserT" $ do
  it "execParserT" $
    let
      parser :: Parser Identity (Integer, Integer, Integer)
      parser = (,,) <$> next <*> next <*> next
    in
      execParserT parser [1, 2, 3] `shouldBe` Identity (1, 2, 3)

  it "fail" $
    execParserT (fail "it fails!") [] `shouldThrow` (== ParserError "it fails!")

  it "parse" $
    let
      parseTest :: Parser IO ()
      parseTest = do
        a1 <- parse [1, 2, 3]
        lift $ a1 `shouldBe` [1, 2, 3]
        a2 <- parse [4, 5, 6]
        lift $ a2 `shouldBe` [4, 5, 6]
        _ <- parse [7, 8, 7]
        lift $ expectationFailure "should fail in the previous line"
    in do
      execParserT parseTest [1..20] `shouldThrow` (== ParserError "fail to parse [7,8,7]")
      execParserT (parse [1, 2, 3]) [] `shouldThrow` (== ParserError "unexpected eof")
