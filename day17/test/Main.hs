module Main (main) where

import qualified Data.Set as S
import Linear.V2 (V2 (..))
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
    [ testApplyJetLeft,
      testApplyJetLeftAtWall,
      testApplyJetLeftAtCave,
      testApplyJetRight,
      testApplyJetRightAtWall,
      testApplyJetRightAtCave,
      testApplyGravityFalling,
      testApplyGravityColliding
    ]

-- a very small rock
tinyRock :: Rock
tinyRock = S.fromList [V2 0 0, V2 1 0]

-- a cave with a rock at (2, 3) and (3, 3)
tinyCave :: Cave
tinyCave = addRock initialCave tinyRock (V2 2 3)

testApplyJetLeft :: Test
testApplyJetLeft =
  TestCase
    ( assertEqual
        "moving left"
        (V2 4 10)
        (applyJet tinyCave tinyRock (V2 5 10) LeftJet)
    )

testApplyJetLeftAtWall :: Test
testApplyJetLeftAtWall =
  TestCase
    ( assertEqual
        "moving left against wall"
        (V2 0 10)
        (applyJet tinyCave tinyRock (V2 0 10) LeftJet)
    )

testApplyJetLeftAtCave :: Test
testApplyJetLeftAtCave =
  TestCase
    ( assertEqual
        "moving left against cave"
        (V2 4 3)
        (applyJet tinyCave tinyRock (V2 4 3) LeftJet)
    )

testApplyJetRight :: Test
testApplyJetRight =
  TestCase
    ( assertEqual
        "moving right"
        (V2 5 10)
        (applyJet tinyCave tinyRock (V2 4 10) RightJet)
    )

testApplyJetRightAtWall :: Test
testApplyJetRightAtWall =
  TestCase
    ( assertEqual
        "moving right against wall"
        (V2 5 10)
        (applyJet tinyCave tinyRock (V2 5 10) RightJet)
    )

testApplyJetRightAtCave :: Test
testApplyJetRightAtCave =
  TestCase
    ( assertEqual
        "moving right against cave"
        (V2 0 3)
        (applyJet tinyCave tinyRock (V2 0 3) RightJet)
    )

testApplyGravityFalling :: Test
testApplyGravityFalling =
  TestCase
    ( assertEqual
        "falling"
        (Just (V2 3 9))
        (applyGravity tinyCave tinyRock (V2 3 10))
    )

testApplyGravityColliding :: Test
testApplyGravityColliding =
  TestCase
    ( assertEqual
        "falling"
        Nothing
        (applyGravity tinyCave tinyRock (V2 3 4))
    )
