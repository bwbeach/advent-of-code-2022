module MyLib (reachable) where

import qualified Data.Set as S

-- | Returns the set of nodes reachable from a starting node.
--
-- This is the third time I've implemented this for this Advent of Code.
-- It must be in a library somewhere, but I haven't found it yet.
reachable :: Ord a => (a -> [a]) -> a -> S.Set a
reachable neighbors start =
  go S.empty (S.singleton start)
  where
    go setSoFar toAdd =
      if S.null toAdd
        then setSoFar
        else go newSet newNodes
      where
        newSet = S.union setSoFar toAdd
        newNodes = S.fromList . filter (notInSet newSet) $ candidates
        notInSet s a = not (S.member a s)
        candidates = [n | s <- S.toList toAdd, n <- neighbors s]
