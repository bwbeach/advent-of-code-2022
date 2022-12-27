-- | Dijkstra's algorithm for computing distance matrix for a graph.
--
-- TODO: put everything but `distances` in an "Internal" module.
module Dijkstra (distances, buildMatrix, State, initialState, stateFromLists, updateDistance) where

import qualified Data.Graph.DGraph as DG
import qualified Data.Graph.Types as GT
import Data.Hashable (Hashable)
import qualified Data.Map.Strict as M (Map, empty, fromList, insert, lookup, toList)
import Debug.Trace
import qualified PriorityQueue as PQ (PriorityQueue, delete, empty, fromList, insert, peek)

-- | Given a list of nodes and a function that returns a node's (neighbors, cost) pairs,
--   returns a function that returns the cost to go from one node to another.
--
--   The cost of going from a node to itself is always 0.
distances :: (Ord a, Show a, Hashable a) => [a] -> (a -> [(a, Int)]) -> DG.DGraph a Int
distances ns getNeighbors = DG.fromArcsList (buildMatrix ns getNeighbors)

-- | Builds the matrix of distances
buildMatrix :: (Ord a, Show a) => [a] -> (a -> [(a, Int)]) -> [GT.Arc a Int]
buildMatrix ns getNeighbors = concatMap (buildEntriesForNode getNeighbors) ns

buildEntriesForNode :: (Ord a, Show a) => (a -> [(a, Int)]) -> a -> [GT.Arc a Int]
buildEntriesForNode getNeighbors start =
  let entriesFromStart = dijkstra (initialState start) getNeighbors
   in map (uncurry (GT.Arc start)) entriesFromStart

-- | Recursive implementation of Dijkstra's algorithm
--
--   The `unvisited` "set" is actually a priority queue of (distance, node).
--   It contains only nodes that have distances assigned.
--
--   The `nodeToDistance` is a map from node to (possibly tentative) distance.
--
--   Both data structures must always be updated together.
dijkstra :: (Ord a, Show a) => State a -> (a -> [(a, Int)]) -> [(a, Int)]
dijkstra state0 getNeighbors =
  case traceIt "PPP" (peekNextUnvisited (traceIt "SSS" state0)) of
    Nothing -> M.toList (nodeToDistance state0)
    Just (d, n) ->
      let state1 = deleteUnvisited (d, n) state0
          neighbors = getNeighbors n
          state2 = foldl (updateNeighbor d) state1 neighbors
       in dijkstra state2 getNeighbors

traceIt :: Show a => String -> a -> a
traceIt label x = trace (label ++ " " ++ show x) x

updateNeighbor :: Ord a => Int -> State a -> (a, Int) -> State a
updateNeighbor d0 state (n, d) =
  updateDistance n (d0 + d) state

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
  deriving (Eq, Show)

stateFromLists :: Ord a => [(Int, a)] -> [(a, Int)] -> State a
stateFromLists u ntd =
  State
    { unvisited = PQ.fromList u,
      nodeToDistance = M.fromList ntd
    }

-- | Initial state, with the starting node at distance 0 on the unvisited list
initialState :: Ord a => a -> State a
initialState start =
  State
    { unvisited = PQ.insert (0, start) PQ.empty,
      nodeToDistance = M.insert start 0 M.empty
    }

peekNextUnvisited :: Ord a => State a -> Maybe (Int, a)
peekNextUnvisited s = PQ.peek (unvisited s)

deleteUnvisited :: Ord a => (Int, a) -> State a -> State a
deleteUnvisited item s = s {unvisited = PQ.delete item (unvisited s)}

-- | Update the distance to a node
--
-- Updating the distance to a node adds it to the unvisited list
-- if it wasn't there before.  If it was there before, it's priority
-- changes based on the new distance.
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
