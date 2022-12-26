-- | Dijkstra's algorithm for computing distance matrix for a graph.
module Dijkstra (distances) where

import qualified Data.Map.Strict as M (Map, empty, fromList, insert, lookup)
import qualified Data.Set as S (Set, empty, insert, member)
import qualified PriorityQueue as PQ (PriorityQueue, delete, empty, insert, null, peek)

-- | Given a list of nodes and a function that returns a node's (neighbors, cost) pairs,
--   returns a function that returns the cost to go from one node to another.
--
--   The cost of going from a nod to itself is always 0.
distances :: Ord a => [a] -> (a -> [(a, Int)]) -> (a -> a -> Maybe Int)
distances ns getNeighbors = getDistance (buildMatrix ns getNeighbors)

-- | Returns the cost to go from node `a` to node `b`, if there is a path.
getDistance :: Ord a => M.Map (a, a) Int -> a -> a -> Maybe Int
getDistance m a b = M.lookup (a, b) m

-- | Builds the matrix of distances
buildMatrix ns getNeighbors = M.fromList (concatMap (buildEntriesForNode getNeighbors) ns)

buildEntriesForNode :: p1 -> p2 -> [a]
buildEntriesForNode getNeighbors n = [] -- build getNeighbers n

-- | Recursive implementation of Dijkstra's algorithm
--
--   The `unvisited` "set" is actually a priority queue of (distance, node).
--   It contains only nodes that have distances assigned.
--
--   The `nodeToDistance` is a map from node to (possibly tentative) distance.
--
--   Both data structures must always be updated together.
dijkstra :: (Ord a) => State a -> (a -> [(a, Int)]) -> [(a, Int)]
dijkstra state0 getNeighbors =
  case peekNextUnvisited state0 of
    Nothing -> makeEntries nodeToDistance
    Just (d, n) ->
      let state1 = deleteUnvisited (d, n) state0
          neighbors = getNeighbors n
          state2 = foldl (updateNeighbor d) state1 neighbors
       in []

updateNeighbor :: Ord a => Int -> State a -> (a, Int) -> State a
updateNeighbor d0 state (n, d) =
  updateDistance n (d0 + d) state

-- TODO
makeEntries :: p -> [a]
makeEntries _ = []

-- | State of Dijkstra's algorithm
--
--   The `unvisited` "set" is actually a priority queue of (distance, node).
--   It contains only nodes that have distances assigned.
--
--   The `nodeToDistance` is a map from node to (possibly tentative) distance.
data State a = State
  { unvisited :: PQ.PriorityQueue (Int, a),
    nodeToDistance :: M.Map a Int
  }

initialState start =
  State
    { unvisited = PQ.insert (0, start) PQ.empty,
      nodeToDistance = M.insert start 0 M.empty
    }

peekNextUnvisited :: Ord a => State a -> Maybe (Int, a)
peekNextUnvisited s = PQ.peek (unvisited s)

deleteUnvisited :: Ord a => (Int, a) -> State a -> State a
deleteUnvisited item s = s {unvisited = PQ.delete item (unvisited s)}

takeNextUnvisited :: Ord a => State a -> (Maybe (Int, a), State a)
takeNextUnvisited s =
  case PQ.peek (unvisited s) of
    Nothing -> (Nothing, s)
    Just item -> (Just item, s {unvisited = PQ.delete item (unvisited s)})

-- | Update the distance to a node
--
-- Updates both data structures inside the state
updateDistance :: Ord a => a -> Int -> State a -> State a
updateDistance n d s =
  let -- get the initial state of the two structures
      u0 = unvisited s
      ntd0 = nodeToDistance s
      -- remove the old value from `unvisited`, if it was there
      u1 = case M.lookup n ntd0 of
        Nothing -> u0
        Just d' -> PQ.delete (d', n) u0
      -- add the new value to both structures
      u2 = PQ.insert (d, n) u1
      ntd2 = M.insert n d ntd0
   in s {unvisited = u2, nodeToDistance = ntd2}

-- makeEntries nodeToDistance = []
