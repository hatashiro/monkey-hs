module LexerSpec where

import Test.Hspec

spec :: IO ()
spec = hspec $
  describe "lexer" $ do
    it "test" $ do
      1 `shouldBe` 1
