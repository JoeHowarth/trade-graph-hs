
import Test.Tasty
import Test.Tasty.HUnit
import Lib

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" []

agentStepTests = testGroup "Agent step related tests"



exampleTests = testGroup "Example Unit Tests"
  [ testCase "List comparison (different length)" $
      [1, 2, 3] `compare` [1,2] @?= GT

  -- the following test does not hold
  , testCase "List comparison (same length)" $
      [1, 2, 3] `compare` [1,2,2] @?= GT
  ]
