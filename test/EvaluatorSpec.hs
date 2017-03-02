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
      eval' "!!true" `shouldEvalTo` true
      eval' "!!false" `shouldEvalTo` false
      eval' "!5" `shouldFail` EvalError "5 is not a bool"
      eval' "!1" `shouldFail` EvalError "1 is not a bool"
      eval' "!0" `shouldFail` EvalError "0 is not a bool"
      eval' "!!5" `shouldFail` EvalError "5 is not a bool"
      eval' "!!0" `shouldFail` EvalError "0 is not a bool"
      -- the prefix +
      eval' "+1" `shouldEvalTo` OInt 1
      eval' "+5" `shouldEvalTo` OInt 5
      eval' "+20" `shouldEvalTo` OInt 20
      eval' "+true" `shouldFail` EvalError "true is not a number"
      eval' "+false" `shouldFail` EvalError "false is not a number"
      -- the prefix -
      eval' "-1" `shouldEvalTo` OInt (-1)
      eval' "-5" `shouldEvalTo` OInt (-5)
      eval' "-20" `shouldEvalTo` OInt (-20)
      eval' "-true" `shouldFail` EvalError "true is not a number"
      eval' "-false" `shouldFail` EvalError "false is not a number"
