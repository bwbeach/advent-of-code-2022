{-
Advent of Code 2022, Day 16

My strategy is to represent the state of the search explicitly,
and do a best-first search, using the A* search from the
algorithms library.

When a state is at the end of the time period, it is "terminal",
and the searching can stop.  The goal is to find the termal state
with the lowest cost.

Cost is counted as the inverse of the score the problem wants:
it's a measure of the flow NOT happening.

There's no point in moving around in the cave, except to go to a valve
and open it.  So the only action ever considered is
move-to-valve-and-open-it, which takes n seconds to go to the place
where the valve is, plus one second to open the valve.

Also, there's no reason to ever open a valve with a flow rate of 0,
so those aren't even considered.

Time for part 1 with A* search, and no useful heuristic: 8.5s
-}

module Main (main) where

import Algorithm.Search (aStar)
import Data.Graph.DGraph (DGraph, fromArcsList)
import qualified Data.Graph.DGraph as DG
import Data.Graph.Types (Arc (..), Graph (vertices))
import Data.Hashable (Hashable)
import Data.List (sort)
import qualified Data.Map.Strict as M (Map, delete, elems, fromList, keys)
import Data.Maybe (fromJust, mapMaybe)
import Debug.Trace
import Dijkstra (distances)

main :: IO ()
main = do
  runInput "test0.txt"
  runInput "test1.txt"
  runInput "test.txt"
  runInput "input.txt"

runInput :: String -> IO ()
runInput fileName = do
  input <- readFile fileName
  putStrLn fileName
  print . day16 $ input

-- A place in the search space, after having made some moves
-- and opened some valves.
data State = State
  { -- the current time, in seconds since start
    stateRemainingTime :: Int,
    -- where in the cave are we?
    statePos :: String,
    -- the list of valves opened, in reverse order
    stateMoves :: [String],
    {- the rest of the fields could be derived from those above, so are not needed in Eq or Ord -}

    -- The flow that has not happened
    stateCost :: Int,
    -- the remaining valves to open
    stateToOpen :: M.Map String Int,
    -- the graph we're working on
    stateGraph :: DGraph String Int
  }

instance Eq State where
  a == b = stateRemainingTime a == stateRemainingTime b && statePos a == statePos b && stateMoves a == stateMoves b

instance Ord State where
  compare a b =
    let tupleA = (stateRemainingTime a, statePos a, stateMoves a)
     in compare tupleA (stateRemainingTime b, statePos b, stateMoves b)

instance Show State where
  show s = "State " ++ show (stateRemainingTime s) ++ " " ++ statePos s ++ " " ++ show (reverse (stateMoves s)) ++ " - " ++ show (stateCost s) ++ " " ++ show (stateToOpen s)

-- The initial state
initialState :: M.Map String Int -> DGraph String Int -> State
initialState valves graph =
  State
    { stateRemainingTime = 30,
      statePos = "AA",
      -- The cost so far: how much it cost to get TO this state
      stateCost = 0,
      stateMoves = [],
      stateToOpen = valves,
      stateGraph = graph
    }

-- | An estimate of the cost of a solution from the given state.
--
-- May under-estimate the cost, but must never over-estimate the cost.
--
-- TODO: make a better estimate
stateEstimate :: State -> Int
stateEstimate = stateCost

-- Advance to the end of time, not doing anything else.
advanceByDoingNothing :: State -> Maybe State
advanceByDoingNothing s =
  if 0 < stateRemainingTime s
    then
      let deltaT = stateRemainingTime s
          unopenedFlow = sum . M.elems $ stateToOpen s
       in Just
            s
              { stateRemainingTime = 0,
                stateCost = stateCost s + deltaT * unopenedFlow
              }
    else Nothing

-- Advance a state by moving to a room with a valve and opening it.
advanceStateByMoving :: State -> String -> Maybe State
advanceStateByMoving s0 dest =
  let g = stateGraph s0
      distance = fromJust (graphLookup g (statePos s0) dest)
      deltaT = distance + 1
      newTime = stateRemainingTime s0 - deltaT
      unopenedFlow = sum . M.elems $ stateToOpen s0
   in if 0 <= newTime
        then
          Just
            s0
              { stateRemainingTime = newTime,
                statePos = dest,
                stateCost = stateCost s0 + unopenedFlow * deltaT,
                stateMoves = dest : stateMoves s0,
                stateToOpen = M.delete dest (stateToOpen s0)
              }
        else Nothing

day16 :: String -> Int
day16 text =
  let -- read the non-zero valves and their rates
      valves = parseValves text
      -- read the graph as presented
      g0 = parseGraph text
      -- compute the distances between all pairs of nodes
      g1 = distances g0
      -- make the initial state
      s0 = initialState valves g1
      -- find the least-cost path
      (cost, _) = fromJust . solve $ s0
      -- what's the cost of doing nothing, and opening no valves?
      maxCost = 30 * (sum . M.elems $ valves)
   in maxCost - cost

solve :: State -> Maybe (Int, [State])
solve = aStar nextStates transitionCost (const 0) isTerminal

transitionCost :: State -> State -> Int
transitionCost s0 s1 =
  let deltaT = stateRemainingTime s0 - stateRemainingTime s1
      unopened = sum . M.elems $ stateToOpen s0
   in deltaT * unopened

nextStates :: State -> [State]
nextStates s0 =
  mapMaybe advanceByDoingNothing [s0]
    ++ mapMaybe (advanceStateByMoving s0) (M.keys (stateToOpen s0))

isTerminal :: State -> Bool
isTerminal s = stateRemainingTime s == 0

traceIt :: Show a => [Char] -> a -> a
traceIt lbl a = trace (lbl ++ " " ++ show a) a

traceGraph :: (Hashable v, Ord v, Show v, Show e) => String -> DGraph v e -> DGraph v e
traceGraph lbl g = trace (lbl ++ "\n" ++ graphToGrid g) g

-- Returns a map from valve name to flow rate for all non-zero valves.
-- We never turn on the valves that have 0, so they don't matter.
parseValves :: String -> M.Map String Int
parseValves = M.fromList . filter (\(_, n) -> n /= 0) . map parseValve . lines

parseValve :: String -> (String, Int)
parseValve s0 =
  let s1 = map (replaceChar '=' ' ' . replaceChar ';' ' ') s0
      ws = words s1
   in (ws !! 1, read (ws !! 5))

parseGraph :: String -> DGraph String Int
parseGraph = fromArcsList . concatMap parseArc . lines

parseArc :: String -> [Arc String Int]
parseArc s0 =
  let ws = words . map (replaceChar ',' ' ') $ s0
      src = ws !! 1
      dests = drop 9 ws
   in map (\dest -> Arc src dest 1) dests

replaceChar :: Char -> Char -> Char -> Char
replaceChar a b c = if c == a then b else c

graphToGrid :: (Hashable v, Ord v, Show v, Show e) => DGraph v e -> String
graphToGrid g =
  let vs = sort . vertices $ g
      header = "" : map show vs
      dataRows = map (makeDataRow g vs) vs
      rows = header : dataRows
   in formatGrid rows

makeDataRow :: (Hashable v, Show v, Show e) => DGraph v e -> [v] -> v -> [String]
makeDataRow g vs v =
  show v : map (maybe "" show . graphLookup g v) vs

-- | Returns the label on one edge in the graph.
--
-- This function is incomplete, and only covers the case where there
-- are 0 or 1 edges between the two specified vertices.
--
-- TODO: there must be a better way to do this
graphLookup :: (Eq v, Hashable v) => DGraph v e -> v -> v -> Maybe e
graphLookup g v w =
  listToMaybe . mapMaybe (checkArc v w) $ DG.outboundingArcs g v

checkArc :: (Eq v) => v -> v -> Arc v e -> Maybe e
checkArc v w (Arc v' w' e) =
  if v == v' && w == w'
    then Just e
    else Nothing

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe [a] = Just a
listToMaybe _ = error "more than one item in list"

showMaybe :: Show a => Maybe a -> String
showMaybe = maybe "" show

formatGrid :: [[String]] -> String
formatGrid rows =
  let cellWidth = maximum . map length . concat $ rows
   in unlines . map (formatLine cellWidth) $ rows

formatLine :: Int -> [String] -> String
formatLine cellWidth = unwords . map (leftPad cellWidth)

leftPad :: Int -> String -> String
leftPad n s =
  let needed = n - length s
   in replicate needed ' ' ++ s
