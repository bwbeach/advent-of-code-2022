module Main (main) where

import qualified Data.Set as S
import MyLib
import qualified System.Exit as Exit
import Test.HUnit (Test (..), assertEqual, failures, runTestTT)

main :: IO ()
main = do
  result <- runTestTT tests
  if failures result > 0 then Exit.exitFailure else Exit.exitSuccess

tests :: Test
tests =
  TestList
    [testReachable]

testReachable :: Test
testReachable =
  TestCase
    ( assertEqual
        "reachable"
        (S.fromList ["a", "b", "c", "d"])
        (reachable neighbors "a")
    )

neighbors :: String -> [String]
neighbors "a" = ["b", "d"]
neighbors "b" = ["c", "d"]
neighbors "c" = []
neighbors "d" = ["a"]
neighbors n = error ("bad node: " ++ n)