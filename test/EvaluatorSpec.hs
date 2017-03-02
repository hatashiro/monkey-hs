module EvaluatorSpec where

import Lexer (lex)
import Parser (parse)
import Evaluator (eval)
import Evaluator.Object
import Evaluator.Types
import Text.RawString.QQ
import Utils (unsafeFromRight)

import Test.Hspec

eval' :: Text -> Either EvalError Object
eval' = eval . unsafeFromRight . (lex >=> parse)

spec :: IO ()
spec = do
  describe "evaluator" $ do
    it ".." $ do
      1 `shouldBe` 1
