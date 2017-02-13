module ParserTSpec where

import Protolude

import Common.ParserT

import Test.Hspec

newtype E = E Text deriving (Show, Typeable)
instance Exception E

type Parser = ParserT [Integer] E Identity

spec :: Spec
spec = describe "ParserT" $ do
  it "execParserT" $ do
    execParserT parser [1, 2, 3] `shouldBe` Identity (1, 2, 3)
    where
    parser :: Parser (Integer, Integer, Integer)
    parser = (,,) <$> next <*> next <*> next
