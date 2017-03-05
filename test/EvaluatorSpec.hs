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

mapDecl :: Text
mapDecl = [r|
let map = fn(f, arr) {
  if (len(arr) == 0) {
    []
  } else {
    let h = head(arr);
    cons(f(h), map(f, tail(arr)));
  }
};
|]

reduceDecl :: Text
reduceDecl = [r|
let reduce = fn(f, init, arr) {
  if (len(arr) == 0) {
    init
  } else {
    let newInit = f(init, head(arr));
    reduce(f, newInit, tail(arr));
  }
};
|]

hash1 :: Text
hash1 = [r|
let double = fn(x) {
  x * 2;
};
let arr = [1, 2, 3, 4];
let h = {
  "one": 10 - 9,
  "two": 8 / 4,
  3: arr[2],
  4: double(2),
  true: if (10 > 8) { true } else { false },
  false: "hello" == "world"
};
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

    it "string" $ do
      eval' [r|"foobar"|] `shouldEvalTo` OString "foobar"
      eval' [r|"foo bar"|] `shouldEvalTo` OString "foo bar"
      eval' [r|"foo\nbar"|] `shouldEvalTo` OString "foo\nbar"
      eval' [r|"foo\tbar"|] `shouldEvalTo` OString "foo\tbar"
      eval' [r|"foo\"bar"|] `shouldEvalTo` OString "foo\"bar"
      eval' [r|"foo" + "bar"|] `shouldEvalTo` OString "foobar"
      eval' [r|"foo" + " " + "bar"|] `shouldEvalTo` OString "foo bar"
      eval' [r|"foo" - "bar"|] `shouldFail` EvalError "\"foo\" is not a number"

    it "array" $ do
      eval' "[1, 2, 3, 4]" `shouldEvalTo` OArray [ OInt 1
                                                 , OInt 2
                                                 , OInt 3
                                                 , OInt 4
                                                 ]
      eval' "let double = fn(x) { x * 2};[1, double(2), 3 * 3, 4 - 3]" `shouldEvalTo`
        OArray [ OInt 1, OInt 4, OInt 9, OInt 1]
      eval' "[1, 2, 3][0]" `shouldEvalTo` OInt 1
      eval' "[1, 2, 3][1]" `shouldEvalTo` OInt 2
      eval' "[1, 2, 3][2]" `shouldEvalTo` OInt 3
      eval' "let i = 0; [1][i];" `shouldEvalTo` OInt 1
      eval' "[1, 2, 3][1 + 1];" `shouldEvalTo` OInt 3
      eval' "let myArray = [1, 2, 3]; myArray[2];" `shouldEvalTo` OInt 3
      eval' "let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2];" `shouldEvalTo` OInt 6
      eval' "let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i];" `shouldEvalTo` OInt 2
      eval' "[1, 2, 3][3]" `shouldEvalTo` nil
      eval' "[1, 2, 3][-1]" `shouldEvalTo` nil

    it "built-in functions" $ do
      -- len
      eval' "len(\"hello world!\")" `shouldEvalTo` OInt 12
      eval' "len(\"\")" `shouldEvalTo` OInt 0
      eval' "len(\"Hey Bob, how ya doin?\")" `shouldEvalTo` OInt 21
      eval' "len(3)" `shouldFail` EvalError "invalid arguments for [built-in function: len]: [3]"
      eval' "len(\"hello\", \"world\")" `shouldFail` EvalError "wrong number of arguments: 1 expected but 2 given"
      eval' "len([])" `shouldEvalTo` OInt 0
      eval' "len([1, 2, 3, 4])" `shouldEvalTo` OInt 4
      -- head
      eval' "head([1])" `shouldEvalTo` OInt 1
      eval' "head([1, 2, 3, 4])" `shouldEvalTo` OInt 1
      eval' "head([])" `shouldFail` EvalError "invalid arguments for [built-in function: head]: empty array"
      -- tail
      eval' "tail([1])" `shouldEvalTo` OArray []
      eval' "tail([1, 2, 3, 4])" `shouldEvalTo` OArray [OInt 2, OInt 3, OInt 4]
      eval' "tail([])" `shouldFail` EvalError "invalid arguments for [built-in function: tail]: empty array"
      -- cons
      eval' "cons(1, [])" `shouldEvalTo` OArray [OInt 1]
      eval' "cons(1, [2, 3, 4])" `shouldEvalTo` OArray [OInt 1, OInt 2, OInt 3, OInt 4]

    it "map & reduce" $ do
      eval' (mapDecl <> "let double = fn(x) { x * 2 }; map(double, [1, 2, 3, 4])") `shouldEvalTo`
        OArray [OInt 2, OInt 4, OInt 6, OInt 8]
      eval' (reduceDecl <> "let add = fn(x, y) { x + y }; reduce(add, 0, [1, 2, 3, 4, 5])") `shouldEvalTo`
        OInt 15

    it "hash" $ do
      eval' (hash1 <> "h[\"one\"]") `shouldEvalTo` OInt 1
      eval' (hash1 <> "let s = \"two\"; h[s]") `shouldEvalTo` OInt 2
      eval' (hash1 <> "h[3]") `shouldEvalTo` OInt 3
      eval' (hash1 <> "h[2 + 2]") `shouldEvalTo` OInt 4
      eval' (hash1 <> "h[true]") `shouldEvalTo` true
      eval' (hash1 <> "h[5 < 1]") `shouldEvalTo` false
      eval' (hash1 <> "h[100]") `shouldEvalTo` nil
      eval' (hash1 <> "h[[]]") `shouldFail` EvalError "[] is not hashable"
      eval' "3[true];" `shouldFail` EvalError "unexpected index target: 3"
