module Main (main) where

import Data.Maybe (fromJust)
import qualified PriorityQueue as PQ (PriorityQueue, delete, empty, insert, null, peek, size)
import qualified System.Exit as Exit
import Test.HUnit (Test (..), assertEqual, failures, runTestTT)

main :: IO ()
main = do
  result <- runTestTT tests
  if failures result > 0 then Exit.exitFailure else Exit.exitSuccess

test1 :: Test
test1 = TestCase (assertEqual "size of empty set should be 0" 0 (PQ.size (PQ.empty :: PQ.PriorityQueue Int)))

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

tests :: Test
tests =
  TestList
    [ TestLabel "emptySet" test1,
      testInsert,
      testDequeue
    ]
