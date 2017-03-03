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

    it "infix op" $ do
      -- algebra
      eval' "5 + 5 + 5 + 5 - 10" `shouldEvalTo` OInt 10
      eval' "2 * 2 * 2 * 2 * 2" `shouldEvalTo` OInt 32
      eval' "-50 + 100 + -50" `shouldEvalTo` OInt 0
      eval' "5 * 2 + 10" `shouldEvalTo` OInt 20
      eval' "5 + 2 * 10" `shouldEvalTo` OInt 25
      eval' "20 + 2 * -10" `shouldEvalTo` OInt 0
      eval' "50 / 2 * 2 + 10" `shouldEvalTo` OInt 60
      eval' "2 * (5 + 10)" `shouldEvalTo` OInt 30
      eval' "3 * 3 * 3 + 10" `shouldEvalTo` OInt 37
      eval' "3 * (3 * 3) + 10" `shouldEvalTo` OInt 37
      eval' "(5 + 10 * 2 + 15 / 3) * 2 + -10" `shouldEvalTo` OInt 50
      -- logical algebra
      eval' "1 < 2" `shouldEvalTo` true
      eval' "1 > 2" `shouldEvalTo` false
      eval' "1 < 1" `shouldEvalTo` false
      eval' "1 > 1" `shouldEvalTo` false
      eval' "1 == 1" `shouldEvalTo` true
      eval' "1 != 1" `shouldEvalTo` false
      eval' "1 == 2" `shouldEvalTo` false
      eval' "1 != 2" `shouldEvalTo` true
      -- combination
      eval' "(1 < 2) == true" `shouldEvalTo` true
      eval' "(1 < 2) == false" `shouldEvalTo` false
      eval' "(1 > 2) == true" `shouldEvalTo` false
      eval' "(1 > 2) == false" `shouldEvalTo` true
