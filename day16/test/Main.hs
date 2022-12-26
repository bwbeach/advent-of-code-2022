module Main (main) where

import MyLib (add1)
import qualified System.Exit as Exit
import Test.HUnit (Test (..), assertEqual, failures, runTestTT)

main :: IO ()
main = do
  result <- runTestTT tests
  if failures result > 0 then Exit.exitFailure else Exit.exitSuccess

test1 :: Test
test1 = TestCase (assertEqual "should return 3" 3 (add1 2))

tests :: Test
tests = TestList [TestLabel "test1" test1]
