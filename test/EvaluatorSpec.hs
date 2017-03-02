module EvaluatorSpec where

import Protolude

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

shouldEvalTo e r = e `shouldBe` Right r
shouldFail e l = e `shouldBe` Left l

spec :: Spec
spec = do
  describe "evaluator" $ do
    it "simple int" $ do
      eval' "5" `shouldEvalTo` OInt 5
      eval' "10" `shouldEvalTo` OInt 10

    it "simple bool" $ do
      eval' "true" `shouldEvalTo` true
      eval' "false" `shouldEvalTo` false

    it "prefix op" $ do
      -- !, the bang operator
      eval' "!true" `shouldEvalTo` false
      eval' "!false" `shouldEvalTo` true
      eval' "!5" `shouldEvalTo` false
      eval' "!1" `shouldEvalTo` false
      eval' "!0" `shouldEvalTo` true
      eval' "!!true" `shouldEvalTo` true
      eval' "!!false" `shouldEvalTo` false
      eval' "!!5" `shouldEvalTo` true
      eval' "!!0" `shouldEvalTo` false
