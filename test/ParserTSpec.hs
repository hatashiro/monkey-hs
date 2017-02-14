module ParserTSpec where

import Protolude hiding (one, many)

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
        a1 <- string [1, 2, 3]
        lift $ a1 `shouldBe` [1, 2, 3]
        a2 <- string [4, 5, 6]
        lift $ a2 `shouldBe` [4, 5, 6]
        _ <- string [7, 8, 7]
        lift $ expectationFailure "should fail in the previous line"
    in do
      execParserT parseTest [1..20] `shouldThrow` (== ParserError "fail to parse [7,8,7]")
      execParserT (string [1, 2, 3]) [] `shouldThrow` (== ParserError "unexpected end of stream")

  it "<|>" $ do
    execParserT (string [1, 2, 3] <|> string [4, 5, 6]) [1..6] `shouldBe` Identity [1, 2, 3]
    execParserT (string [1, 2, 3] <|> string [4, 5, 6]) [4, 5, 6, 1, 2, 3] `shouldBe` Identity [4, 5, 6]
    execParserT (empty <|> string [4, 5, 6]) [4, 5, 6, 1, 2, 3] `shouldBe` Identity [4, 5, 6]
    execParserT (string [4, 5, 6] <|> empty) [4, 5, 6, 1, 2, 3] `shouldBe` Identity [4, 5, 6]
    execParserT (string [1, 2, 3] <|> empty) [4, 5, 6, 1, 2, 3] `shouldThrow` (== ParserError "empty")
    execParserT (empty <|> string [1, 2, 3]) [4, 5, 6, 1, 2, 3] `shouldThrow` (== ParserError "fail to parse [1,2,3]")
    execParserT (string [1, 2, 3] <|> string [4, 5, 6]) [10..] `shouldThrow` (== ParserError "fail to parse [4,5,6]")
    execParserT (string [1, 2, 3] <|> string [4, 5, 6]) [] `shouldThrow` (== ParserError "unexpected end of stream")

  it "choose" $ do
    execParserT (
      choose [ string [1, 2, 3]
             , string [4, 5, 6, 7]
             , string [4, 5, 6]
             ]
      )
      [1..] `shouldBe` Identity [1, 2, 3]
    execParserT (
      choose [ string [1, 2, 3]
             , string [4, 5, 6, 7]
             , string [4, 5, 6]
             ]
      )
      [4..] `shouldBe` Identity [4, 5, 6, 7]
    execParserT (
      choose [ string [1, 2, 3]
             , string [4, 5, 6, 7]
             , string [4, 5, 6]
             ]
      )
      [4, 5, 6, 5] `shouldBe` Identity [4, 5, 6]
    execParserT (
      choose [ string [1, 2, 3]
             , string [4, 5, 6, 7]
             , string [4, 5, 6]
             ]
      )
      [5..] `shouldThrow` (== ParserError "fail to parse [4,5,6]")
    execParserT (
      choose []
      )
      [5..] `shouldThrow` (== ParserError "empty")

  it "predicate" $ do
    execParserT (predicate (< 5)) [1..] `shouldBe` Identity 1
    execParserT (predicate (< 5)) [4..] `shouldBe` Identity 4
    execParserT (predicate (< 5)) [5..] `shouldThrow` (== ParserError "unexpected 5")

  it "many" $ do
    let lt5 = predicate (< 5)
    execParserT (many lt5) [1..4] `shouldBe` Identity [1..4]
    execParserT (many lt5) [1..] `shouldBe` Identity [1..4]
    execParserT (many lt5) [4..] `shouldBe` Identity [4]
    execParserT (many lt5) [5..] `shouldBe` Identity []
