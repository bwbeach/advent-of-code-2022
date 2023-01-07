module Main (main) where

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
    [ testLex,
      testTimeUntilN,
      testBestPossible0,
      testBestPossible1,
      testBestPossible2,
      testBestPossible3,
      testTimeUntilClay
    ]

testLex :: Test
testLex =
  TestCase
    ( assertEqual
        "adventLex"
        [Token "a" 1 1, Token "3" 1 3, Token "." 1 4, Token "=" 2 1, Token "400" 2 2]
        (adventLex "a 3.\n=400")
    )

testTimeUntilN :: Test
testTimeUntilN =
  TestCase
    ( assertEqual
        "reachable"
        [1, 2, 2, 3, 3, 3, 4, 9]
        (map timeUntilN [1, 2, 3, 4, 5, 6, 7, 45])
    )

recipe1 :: Recipe
recipe1 =
  Recipe
    ( M.fromList
        [ (Robot Geode, [(45, Res Obsidian)]), -- 45! it takes a lot of obsidian
          (Robot Obsidian, [(1, Res Clay)]),
          (Robot Clay, [(1, Res Ore)])
        ]
    )

nodeWithTwentyGeodes :: Node
nodeWithTwentyGeodes =
  Node
    { nodeCounts =
        M.fromList
          [ (Res Geode, 20),
            (Robot Ore, 1)
          ],
      nodeTimeLeft = 0
    }

testBestPossible0 :: Test
testBestPossible0 =
  TestCase
    ( assertEqual
        "possible0"
        20
        (bestPossibleScore recipe1 nodeWithTwentyGeodes)
    )

nodeWithOneGeodeRobot :: Int -> Node
nodeWithOneGeodeRobot t =
  Node
    { nodeCounts =
        M.fromList
          [ (Robot Geode, 1),
            (Robot Ore, 1)
          ],
      nodeTimeLeft = t
    }

-- start with one geode robot, with one time unit left.  the robot should get one geode
testBestPossible1 :: Test
testBestPossible1 =
  TestCase
    ( assertEqual
        "possible1"
        1
        (bestPossibleScore recipe1 (nodeWithOneGeodeRobot 1))
    )

-- start with one geode robot, with 3 time units left.
-- there will be one geode robot in the first time interval (1 geode),
-- two in the second (2 geodes) and 3 in the third (3 geodes), for
-- a total of six.
testBestPossible2 :: Test
testBestPossible2 =
  TestCase
    ( assertEqual
        "possible2"
        6
        (bestPossibleScore recipe1 (nodeWithOneGeodeRobot 3))
    )

nodeWithOre :: Node
nodeWithOre = Node {nodeCounts = M.fromList [(Robot Ore, 1)], nodeTimeLeft = 15}

-- start with one ore robot, which will produce an ore at time 1,
-- which will allow a clay robot at time 2, which will produce a clay at time 3,
-- which will allow an obsidian robot at time 4, and an obsidian at time 5,
-- which means that there will be 45 obsidian at time 13, and then one geode robot at time 14
-- and one geode at time 15.
testBestPossible3 :: Test
testBestPossible3 =
  TestCase
    ( assertEqual
        "possible3"
        1
        (bestPossibleScore recipe1 nodeWithOre)
    )

testTimeUntilClay :: Test
testTimeUntilClay =
  TestCase
    ( assertEqual
        "timeUntilClay"
        3
        (timeUntil recipe1 nodeWithOre (Res Clay))
    )
