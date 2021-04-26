module TestUtils where

import Test.HUnit

printTests :: Test -> IO ()
printTests test = runTestTT test >>= print

testList :: [Assertion] -> Test
testList assertions = TestList $ TestCase <$> assertions
