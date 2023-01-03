module Main (main) where

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
    [ testMoveLeft,
      testMoveLeftNothing,
      testMoveRight,
      testMoveRightNothing,
      testAddRockAbove,
      testAddRockBelow,
      testAddRockMerge,
      testAddRockCollide
    ]

testMoveLeft :: Test
testMoveLeft =
  TestCase
    ( assertEqual
        "move rock left"
        [" #", "##"]
        (moveRockLeft ["  #", " ##"])
    )

testMoveLeftNothing :: Test
testMoveLeftNothing =
  TestCase
    ( assertEqual
        "move rock left - nothing"
        ["# #", " ##"]
        (moveRockLeft ["# #", " ##"])
    )

testMoveRight :: Test
testMoveRight =
  TestCase
    ( assertEqual
        "move rock right"
        ["  #", " ##"]
        (moveRockRight [" #", "##"])
    )

testMoveRightNothing :: Test
testMoveRightNothing =
  TestCase
    ( assertEqual
        "move rock right"
        [" ######", "##"]
        (moveRockRight [" ######", "##"])
    )

testAddRockAbove :: Test
testAddRockAbove =
  TestCase
    ( assertEqual
        "add rock above"
        (Just [" #", "", "#######"])
        (addRock [" #"] 2 ["#######"])
    )

testAddRockBelow :: Test
testAddRockBelow =
  TestCase
    ( assertEqual
        "add rock above"
        (Just ["#######", "", " #"])
        (addRock [" #"] (negate 2) ["#######"])
    )

testAddRockMerge :: Test
testAddRockMerge =
  TestCase
    ( assertEqual
        "add rock above"
        (Just ["  #", "# #", "  #"])
        (addRock ["  #", "  #", "  #"] 1 ["#"])
    )

testAddRockCollide :: Test
testAddRockCollide =
  TestCase
    ( assertEqual
        "add rock above"
        Nothing
        (addRock ["  #", "  #", "  #"] 1 ["###"])
    )