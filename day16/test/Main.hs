module Main (main) where

import Data.Maybe (fromJust)
import qualified Dijkstra as D
import qualified PriorityQueue as PQ (PriorityQueue, delete, empty, insert, null, peek, size)
import qualified System.Exit as Exit
import Test.HUnit (Test (..), assertEqual, failures, runTestTT)

main :: IO ()
main = do
  result <- runTestTT tests
  if failures result > 0 then Exit.exitFailure else Exit.exitSuccess

--
-- PriorityQueue
--

testEmpty :: Test
testEmpty = TestCase (assertEqual "size of empty set should be 0" 0 (PQ.size (PQ.empty :: PQ.PriorityQueue Int)))

testInsert :: Test
testInsert =
  let empty = PQ.empty :: PQ.PriorityQueue Int
   in TestCase (assertEqual "size after adding one thing" 1 (PQ.size (PQ.insert 1 empty)))

dequeueAll :: Ord a => PQ.PriorityQueue a -> [a]
dequeueAll pq
  | PQ.null pq = []
  | otherwise = let a = fromJust (PQ.peek pq) in a : dequeueAll (PQ.delete a pq)

insertAll :: Ord a => [a] -> PQ.PriorityQueue a -> PQ.PriorityQueue a
insertAll as pq = foldl (flip PQ.insert) pq as

testDequeue :: Test
testDequeue =
  let empty = PQ.empty :: PQ.PriorityQueue Int
   in TestCase (assertEqual "items should be dequeued in order" [1, 2, 3, 4, 5] (dequeueAll (insertAll [3, 2, 4, 5, 1] empty)))

--
-- State
--

testEmptyState :: Test
testEmptyState =
  let expected = D.stateFromLists [(0, "start")] [("start", 0)]
      actual = D.initialState "start"
   in TestCase (assertEqual "empty state" expected actual)

testUpdateDistance :: Test
testUpdateDistance =
  let expected = D.stateFromLists [(0, "s"), (7, "a"), (9, "b")] [("a", 7), ("b", 9), ("s", 0)]
      actual = D.updateDistance "a" 7 . D.updateDistance "b" 9 . D.updateDistance "a" 10 $ D.initialState "s"
   in TestCase (assertEqual "update state" expected actual)

--
-- TEST LIST
--

tests :: Test
tests =
  TestList
    [ -- PriorityQueue
      TestLabel "emptySet" testEmpty,
      testInsert,
      testDequeue,
      -- State
      testEmptyState,
      testUpdateDistance
    ]
