module ParserTSpec where

import Protolude

import Common.ParserT

import Test.Hspec

newtype E = E Text deriving (Show, Typeable)
instance Exception E

type Parser m = ParserT [Integer] E m

spec :: Spec
spec = describe "ParserT" $ do
  it "execParserT" $
    let
      parser :: Parser Identity (Integer, Integer, Integer)
      parser = (,,) <$> next <*> next <*> next
    in
      execParserT parser [1, 2, 3] `shouldBe` Identity (1, 2, 3)

  it "try" $
    let
      tryTest :: Monad m => Parser m ()
      tryTest = do
        return ()
    in
      execParserT tryTest [1..] :: Expectation
