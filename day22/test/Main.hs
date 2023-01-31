module Main (main) where

import Data.Bits
import qualified Data.Map.Strict as M
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
    [testExplore]

testExplore :: Test
testExplore =
  TestCase
    ( assertEqual
        "testExplore"
        (M.fromList (map (\n -> (show n, n)) [0 .. 7]))
        (explore getKey getValue getSuccessors (0 :: Int))
    )
  where
    getKey = show
    getValue s = s
    getSuccessors n = [n `xor` 1, n `xor` 2, n `xor` 4]
