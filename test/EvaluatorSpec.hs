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

eval' :: Text -> IO (Either EvalError Object)
eval' = eval . unsafeFromRight . (lex >=> parse)

shouldEvalTo e r = e >>= (`shouldBe` Right r)
shouldFail e l = e >>= (`shouldBe` Left l)

return1 :: Text
return1 = [r|
if (10 > 1) {
  if (10 > 1) {
    return 10;
  }
  return 1;
}
|]

fn1 :: Text
fn1 = [r|
let add = fn(a, b, c, d) { return a + b + c + d; };
add(1, 2, 3, 4);
|]

fn2 :: Text
fn2 = [r|
let addThree = fn(x) { return x + 3 };
addThree(3);
|]

fn3 :: Text
fn3 = [r|
let max = fn(x, y) { if (x > y) { x } else { y } };
max(5, 10)
|]

fn4 :: Text
fn4 = [r|
let factorial = fn(n) {
  if (n == 0) {
    1
  } else {
    n * factorial(n - 1)
  }
}
factorial(5)
|]

fn5 :: Text
fn5 = [r|
let addThree = fn(x) { return x + 3 };
let callTwoTimes = fn(x, f) { f(f(x)) }
callTwoTimes(3, addThree);
|]

fn6 :: Text
fn6 = [r|
let callTwoTimes = fn(x, f) { f(f(x)) }
callTwoTimes(3, fn(x) { x + 1 });
|]

fn7 :: Text
fn7 = [r|
let newAdder = fn(x) { fn(n) { x + n } };
let addTwo = newAdder(2);
addTwo(2);
|]

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

    it "conditional expr" $ do
      eval' "if (true) { 10 }" `shouldEvalTo` OInt 10
      eval' "if (false) { 10 }" `shouldEvalTo` nil
      eval' "if (1) { 10 }" `shouldFail` EvalError "1 is not a bool"
      eval' "if (1 < 2) { 10 }" `shouldEvalTo` OInt 10
      eval' "if (1 > 2) { 10 }" `shouldEvalTo` nil
      eval' "if (1 < 2) { 10 } else { 20 }" `shouldEvalTo` OInt 10
      eval' "if (1 > 2) { 10 } else { 20 }" `shouldEvalTo` OInt 20

    it "return statement" $ do
      eval' "return 10" `shouldEvalTo` OInt 10
      eval' "return 10; 9" `shouldEvalTo` OInt 10
      eval' "return 2 * 5; 9" `shouldEvalTo` OInt 10
      eval' "9; return 2 * 5; 9" `shouldEvalTo` OInt 10
      eval' return1 `shouldEvalTo` OInt 10

    it "bindings (let & ident)" $ do
      eval' "let a = 5; a;" `shouldEvalTo` OInt 5
      eval' "let a = 5 * 5; a;" `shouldEvalTo` OInt 25
      eval' "let a = 5; let b = a; b;" `shouldEvalTo` OInt 5
      eval' "let a = 5; let b = a; let c = a + b + 5; c;" `shouldEvalTo` OInt 15
      eval' "foobar" `shouldFail` EvalError "identifier not found: foobar"

    it "function def and eval" $ do
      eval' "let identity = fn(x) { x; }; identity(5);" `shouldEvalTo` OInt 5
      eval' "let identity = fn(x) { return x; }; identity(5);" `shouldEvalTo` OInt 5
      eval' "let double = fn(x) { x * 2; }; double(5);" `shouldEvalTo` OInt 10
      eval' "let add = fn(x, y) { x + y; }; add(5, 5);" `shouldEvalTo` OInt 10
      eval' "let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));" `shouldEvalTo` OInt 20
      eval' "fn(x) { x; }(5)" `shouldEvalTo` OInt 5
      eval' "5();" `shouldFail` EvalError "5 is not a function"
      eval' "false();" `shouldFail` EvalError "false is not a function"
      eval' "let add = fn(x, y) { x + y; }; add(1);" `shouldFail` EvalError "wrong number of arguments: 2 expected but 1 given"
      eval' fn1 `shouldEvalTo` OInt 10
      eval' fn2 `shouldEvalTo` OInt 6
      eval' fn3 `shouldEvalTo` OInt 10
      eval' fn4 `shouldEvalTo` OInt 120
      eval' fn5 `shouldEvalTo` OInt 9
      eval' fn6 `shouldEvalTo` OInt 5
      eval' fn7 `shouldEvalTo` OInt 4
      -- special cases
      eval' "let a = 10; let x = fn () { a; }; x();" `shouldEvalTo` OInt 10
      eval' "let x = fn () { a; }; let a = 10; x();" `shouldEvalTo` OInt 10
