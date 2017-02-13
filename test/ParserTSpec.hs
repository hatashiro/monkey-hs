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

  it "try" $
    let
      tryTest :: Monad m => Parser m ()
      tryTest = do
        return ()
    in
      execParserT tryTest [1..] :: Expectation
