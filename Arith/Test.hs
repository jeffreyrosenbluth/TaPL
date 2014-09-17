-- | Tests for Arithmetic Expressions
--   TaPL Chapter 4
--   (c) 2014 Jeffrey Rosenbluth

import Test.Tasty
import Test.Tasty.HUnit
import Eval

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [evalTests]

five :: String
five = "succ(succ(succ(succ(succ 0))))"

showVal :: String -> String
showVal = show . runEval

makeTest :: (TestName, String, String) -> TestTree
makeTest (name, expected, actual) =
  testCase name $ showVal actual `compare` expected @?= EQ

evalData
    = [("true", "true", "true"),
       ("false", "false", "false"),
       ("zero", "0", "0"),
       ("natural numbers", "5", five),
       ("E-IfTrue", "0", "if true then 0 else false"),
       ("E-IfFalse", "false", "if false then 0 else false"),
       ("E-If", "0", "if if false then false else true then 0 else false"),
       ("succ", "1", "succ 0"),
       ("E-Succ", "1", "succ if true then 0 else false"),
       ("E-PredSucc", "0", "pred (succ 0)"),
       ("E-PredZero", "0", "pred 0"),
       ("E-Pred", "0", "pred (pred 0)"),
       ("test.f #4", "1", "succ (pred 0)"),
       ("test.f #5", "false", "iszero (pred (succ (succ 0)))"),
       ("E-IsZeroZero", "true", "iszero 0"),
       ("E-IsZeroSucc", "false", "iszero (succ 0)"),
       ("E-IsZero", "true", "iszero (pred (succ 0))")
      ]

evalTests = testGroup "Eval Tests" (map makeTest evalData)

