{-
Advent of Code 2022, Day 16

My strategy is to represent the state of the search explicitly,
and do a most-likely-to-succeed-first search, based on a scoring
function that gives an upper bound on the score resulting from
a state.  The state with the highest upper bound is proccessed
fist, resulting in a set of new states, usually with lower
upper bounds.

When a state has no more moves available, it is "terminal", and the
upper bound on the score is also the exact score.  When the state
with the highest upper bound is terminal, that's the answer.

There's no point in moving around in the cave, except to go to a valve
and open it.  So the only action ever considered is
move-to-valve-and-open-it, which takes n seconds to go to the place
where the valve is, plus one second to open the valve.

Also, there's no reason to ever open a valve with a flow rate of 0,
so those aren't even considered.
-}

module Main where

import Data.Graph.DGraph (DGraph, fromArcsList)
import qualified Data.Graph.DGraph as DG
import Data.Graph.Types (Arc (..), Graph (vertices))
import Data.Hashable (Hashable)
import Data.List (sort)
import qualified Data.Map.Strict as M (Map, delete, elems, fromList, toDescList, toList)
import Data.Maybe (fromJust, mapMaybe)
import Data.Tuple (swap)
import Debug.Trace
import Dijkstra (distances)
import qualified PriorityQueue as PQ

main :: IO ()
main = do
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
    stateTime :: Int,
    -- where in the cave are we?
    statePos :: String,
    -- the list of valves opened, in reverse order
    stateMoves :: [String],
    {- the rest of the fields could be derived from those above, so are not needed in Eq or Ord -}

    -- the total score from all of the valves that have been
    -- opened so far, including all flows from those valves
    -- up to time 30.
    stateScore :: Int,
    -- the remaining valves to open
    stateToOpen :: M.Map String Int,
    -- the graph we're working on
    stateGraph :: DGraph String Int
  }

instance Eq State where
  a == b = stateTime a == stateTime b && statePos a == statePos b && stateMoves a == stateMoves b

instance Ord State where
  compare a b =
    let tupleA = (stateTime a, statePos a, stateMoves a)
     in compare tupleA (stateTime b, statePos b, stateMoves b)

instance Show State where
  show s = "State " ++ show (stateTime s) ++ " " ++ statePos s ++ " " ++ show (reverse (stateMoves s)) ++ " - " ++ show (stateScore s) ++ " " ++ show (stateToOpen s)

-- The initial state
initialState :: M.Map String Int -> DGraph String Int -> State
initialState valves graph =
  State
    { stateTime = 0,
      statePos = "AA",
      stateScore = 0,
      stateMoves = [],
      stateToOpen = valves,
      stateGraph = graph
    }

-- Advance a state by moving to a room with a valve and opening it.
advanceState :: State -> (String, Int) -> State
advanceState s0 (dest, flow) =
  let g = stateGraph s0
      distance = fromJust (graphLookup g (statePos s0) dest)
      newTime = stateTime s0 + distance + 1
   in s0
        { stateTime = newTime,
          statePos = dest,
          stateScore = stateScore s0 + (30 - newTime) * flow,
          stateMoves = dest : stateMoves s0,
          stateToOpen = M.delete dest (stateToOpen s0)
        }

-- An entry in the queue of states to look at.
-- Ordering is highest upper bound first.
data QueueEntry = QueueEntry
  { qeUpperBound :: Int,
    qeState :: State
  }
  deriving (Eq, Show)

instance Ord QueueEntry where
  compare QueueEntry {qeUpperBound = ub1, qeState = s1} QueueEntry {qeUpperBound = ub2, qeState = s2} =
    compare (ub2, s2) (ub1, s1)

makeQueueEntry :: State -> QueueEntry
makeQueueEntry s = QueueEntry {qeUpperBound = upperBound s, qeState = s}

upperBound :: State -> Int
upperBound s =
  if isTerminal s
    then stateScore s
    else -- TODO: store valves in a more convenient way

      let valvesInOrder = map fst . reverse . sort . map swap . M.toDescList $ stateToOpen s
       in upperBoundHelper (stateScore s) (stateTime s) valvesInOrder

upperBoundHelper :: Int -> Int -> [Int] -> Int
upperBoundHelper s t vs
  | 30 <= t = s
  | otherwise = case vs of
      [] -> s
      (v : vs') -> upperBoundHelper (s + (30 - t) * v) (t + 2) vs'

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
      -- make the initial queue
      q0 = PQ.fromList [makeQueueEntry s0]
   in solve q0

solve :: PQ.PriorityQueue QueueEntry -> Int
solve q0 =
  let entry = fromJust (PQ.peek q0)
      s = qeState entry
      q1 = PQ.delete entry q0
   in if isTerminal s
        then stateScore s
        else solve (addNextSteps s q1)

addNextSteps :: State -> PQ.PriorityQueue QueueEntry -> PQ.PriorityQueue QueueEntry
addNextSteps s pq = foldl addNextStep pq (nextQueueEntries s)

addNextStep :: PQ.PriorityQueue QueueEntry -> QueueEntry -> PQ.PriorityQueue QueueEntry
addNextStep pq qe = PQ.insert qe pq

nextQueueEntries :: State -> [QueueEntry]
nextQueueEntries s = map makeQueueEntry (nextStates s)

nextStates :: State -> [State]
nextStates s0 = map (advanceState s0) (M.toList (stateToOpen s0))

isTerminal :: State -> Bool
isTerminal s =
  let t = stateTime s
      toOpen = stateToOpen s
   in 30 <= stateTime s || null toOpen || 29 - t <= minimum (M.elems toOpen)

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
