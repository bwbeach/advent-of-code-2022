module Main (main) where

import qualified Data.Map.Strict as M
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
    [ testTimeUntilN,
      testBestPossible0,
      testBestPossible1,
      testBestPossible2,
      testBestPossible3,
      testTimeUntilClay,
      testNodeSuccessors,
      testRemoveResources,
      testRemoveResources2,
      testRunRobots
    ]

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
  makeRecipe
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

nodeWithOreRobot :: Node
nodeWithOreRobot = Node {nodeCounts = M.fromList [(Robot Ore, 1)], nodeTimeLeft = 15}

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
        (bestPossibleScore recipe1 nodeWithOreRobot)
    )

testTimeUntilClay :: Test
testTimeUntilClay =
  TestCase
    ( assertEqual
        "timeUntilClay"
        3
        (timeUntil recipe1 nodeWithOreRobot (Res Clay))
    )

nodeWithTenOre :: Node
nodeWithTenOre =
  Node
    { nodeCounts = M.fromList [(Res Ore, 10)],
      nodeTimeLeft = 15
    }

recipe2 :: Recipe
recipe2 =
  makeRecipe
    ( M.fromList
        [ (Robot Geode, [(45, Res Obsidian)]), -- 45! it takes a lot of obsidian
          (Robot Obsidian, [(1, Res Ore)]),
          (Robot Clay, [(2, Res Ore)]),
          (Robot Ore, [(4, Res Ore)])
        ]
    )

testNodeSuccessors :: Test
testNodeSuccessors =
  TestCase
    ( assertEqual
        "nodeSuccessors"
        ( S.fromList
            [ Node {nodeCounts = M.fromList [(Res Ore, 10)], nodeTimeLeft = 14},
              Node {nodeCounts = M.fromList [(Res Ore, 9), (Robot Obsidian, 1)], nodeTimeLeft = 14},
              Node {nodeCounts = M.fromList [(Res Ore, 8), (Robot Clay, 1)], nodeTimeLeft = 14},
              Node {nodeCounts = M.fromList [(Res Ore, 6), (Robot Ore, 1)], nodeTimeLeft = 14}
            ]
        )
        (S.fromList (nodeSuccessors recipe2 nodeWithTenOre))
    )

testRemoveResources :: Test
testRemoveResources =
  TestCase
    ( assertEqual
        "removeResources"
        (Just (Node {nodeCounts = M.fromList [(Res Ore, 9)], nodeTimeLeft = 15}))
        (removeResources nodeWithTenOre [(1, Res Ore)])
    )

testRemoveResources2 :: Test
testRemoveResources2 =
  TestCase
    ( assertEqual
        "removeResources"
        Nothing
        (removeResources nodeWithTenOre [(11, Res Ore)])
    )

testRunRobots :: Test
testRunRobots =
  TestCase
    ( assertEqual
        "runRobots"
        (Node {nodeCounts = M.fromList [(Robot Ore, 1), (Res Ore, 1)], nodeTimeLeft = 15})
        (runRobots nodeWithOreRobot)
    )
