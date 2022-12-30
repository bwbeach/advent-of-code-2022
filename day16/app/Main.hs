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

Time for part 1 with A* search, and no useful heuristic: 12s
Time for part 1 with A* search, and a heuristic: 3.5s
-}

module Main (main) where

import Algorithm.Search (aStar)
import Data.Graph.DGraph (DGraph, fromArcsList)
import qualified Data.Graph.DGraph as DG
import Data.Graph.Types (Arc (..), Graph (vertices))
import Data.Hashable (Hashable)
import Data.List (sort)
import qualified Data.Map.Strict as M (Map, delete, elems, fromList, keys, (!))
import Data.Maybe (catMaybes, fromJust, mapMaybe)
import qualified Data.Set as S (Set, delete, empty, fromList, insert, toList)
import Debug.Trace
import Dijkstra (distances)

main :: IO ()
main = do
  runInput "test0.txt"
  runInput "test1.txt"
  runInput "test2.txt"
  runInput "test.txt"
  runInput "input.txt"

runInput :: String -> IO ()
runInput fileName = do
  input <- readFile fileName
  putStrLn fileName
  runDay16 30 [makeActor "me" 30 "AA"] $ input
  runDay16 26 [makeActor "me" 26 "AA", makeActor "elephant" 26 "AA"] $ input

runDay16 :: Int -> [Actor] -> String -> IO ()
runDay16 duration actors input = do
  let (totalFlow, path) = day16 duration actors input
  print totalFlow
  putStr . unlines . map show $ path

-- An actor (me or the elephant) who is going around opening valves
--
data Actor = Actor
  { -- the name of the actor
    actorName :: String,
    -- the time at which the actor will be available
    actorAvailableTime :: Int,
    -- the location where an actor will be available
    actorLocation :: String,
    -- the flow for the valve that finishes opening when the actor becomes available
    actorFlowIncrease :: Int
  }
  deriving (Eq, Ord)

makeActor :: String -> Int -> String -> Actor
makeActor name avail loc =
  Actor
    { actorName = name,
      actorAvailableTime = avail,
      actorLocation = loc,
      actorFlowIncrease = 0
    }

instance Show Actor where
  show a = actorName a ++ ":" ++ show (actorAvailableTime a) ++ ":" ++ actorLocation a ++ ":" ++ show (actorFlowIncrease a)

-- A place in the search space, after having made some moves
-- and opened some valves.
data State = State
  { -- the current time, in seconds since start
    stateRemainingTime :: Int,
    -- what actors are around to do things?
    stateActors :: S.Set Actor,
    -- The flow that has NOT happened up until now
    stateCost :: Int,
    -- The sum of the costs of the valves that are not open
    stateCostPerSecond :: Int,
    -- the remaining valves to open.  valves are removed as soon as opening is planned.
    stateToOpen :: M.Map String Int
  }
  deriving (Eq, Ord)

instance Show State where
  show s =
    "State "
      ++ show (stateRemainingTime s)
      ++ " "
      ++ show (stateActors s)
      ++ " - "
      ++ show (stateCost s)
      ++ " "
      ++ show (stateToOpen s)
      ++ " "
      ++ show (stateCostPerSecond s)

-- The initial state
initialState :: Int -> [Actor] -> M.Map String Int -> State
initialState remainingTime actors valves =
  State
    { stateRemainingTime = remainingTime,
      stateActors = S.fromList actors,
      stateCost = 0,
      stateCostPerSecond = sum . M.elems $ valves,
      stateToOpen = valves
    }

-- | An estimate of the cost of a solution from the given state.
--
-- May under-estimate the cost, but must never over-estimate the cost.
--
-- At best, we can open one valve every other second, starting with the
-- highest flow valve.
--
-- TODO: use number of actors.  this is a hack that assumes at most 2
stateEstimate :: State -> Int
stateEstimate s =
  let valveRates = reverse . sort . M.elems $ stateToOpen s
      t = stateRemainingTime s
      maxCost = sum valveRates * t
      bestCaseFlow = sum . zipWith (*) [t - 1, t - 2 .. 0] $ valveRates
   in maxCost - bestCaseFlow

-- | Advance a state with no actors to the end of time.
--
-- With no actors, no valves will change state, so the cost rate remains constant.
nextWithNoActors :: State -> Maybe State
nextWithNoActors s =
  if null (stateActors s)
    then
      Just
        s
          { stateRemainingTime = 0,
            stateCost = stateCost s + stateCostPerSecond s * stateRemainingTime s
          }
    else Nothing

-- | Advance to the end of time, not doing anything else.
nextByRemovingActor :: State -> Actor -> Maybe State
nextByRemovingActor s0 actor =
  let s1 = openValve actor s0
   in Just s1 {stateActors = S.delete actor $ stateActors s0}

-- | Opens the valve specified by the given actor
openValve :: Actor -> State -> State
openValve actor s =
  s
    { stateCostPerSecond = stateCostPerSecond s - actorFlowIncrease actor
    }

-- | Advance a state by planning a move for an actor
advanceStateByMoving :: DGraph String Int -> State -> Actor -> String -> Maybe State
advanceStateByMoving g s0 actor dest =
  let s1 = openValve actor s0
      src = actorLocation actor
      distance = fromJust (graphLookup g src dest) -- how far the actor is moving
      actorDuration = distance + 1 -- how long it will take to move and open the valve
      actorDoneTime = stateRemainingTime s1 - actorDuration
      actor1 = actor {actorAvailableTime = actorDoneTime, actorLocation = dest, actorFlowIncrease = stateToOpen s0 M.! dest}
      newActors = S.insert actor1 . S.delete actor $ stateActors s1
   in if 0 < actorDoneTime -- no point in opening a valve if it won't have any time to flow
        then
          Just
            s1
              { stateActors = newActors,
                stateToOpen = M.delete dest (stateToOpen s0)
              }
        else Nothing

-- | Advance a state by moving the clock forward to the time the next actor is available.
advanceStateByChangingClock :: State -> Maybe State
advanceStateByChangingClock s =
  if null (stateActors s)
    then Nothing
    else
      let timeNow = stateRemainingTime s
          maxTime = maximum . map actorAvailableTime . S.toList $ stateActors s
          deltaT = timeNow - maxTime
       in if timeNow <= maxTime
            then Nothing
            else
              Just
                s
                  { stateRemainingTime = maxTime,
                    stateCost = stateCost s + stateCostPerSecond s * deltaT
                  }

day16 :: Int -> [Actor] -> String -> (Int, [State])
day16 remainingTime actors text =
  let -- read the non-zero valves and their rates
      valves = parseValves text
      -- read the graph as presented
      g0 = parseGraph text
      -- compute the distances between all pairs of nodes
      g1 = distances g0
      -- make the initial state
      s0 = initialState remainingTime actors valves
      -- find the least-cost path
      result = fromJust . solve g1 $ s0
      (cost, path) = result
      -- what's the cost of doing nothing, and opening no valves?
      maxCost = remainingTime * (sum . M.elems $ valves)
   in (maxCost - cost, path)

solve :: DGraph String Int -> State -> Maybe (Int, [State])
solve g = aStar (nextStates g) transitionCost stateEstimate isTerminal

-- | Cost of moving from one state to another
-- is the cost of all of the valves that aren't opened yet.
transitionCost :: State -> State -> Int
transitionCost s0 s1 = stateCost s1 - stateCost s0

nextStates :: DGraph String Int -> State -> [State]
nextStates g s0 =
  catMaybes (jumpToEnd ++ changedClock ++ removedActors ++ movedActors)
  where
    jumpToEnd = [nextWithNoActors s0]
    changedClock = [advanceStateByChangingClock s0]
    removedActors =
      [ nextByRemovingActor s0 actor
        | actor <- S.toList $ stateActors s0,
          actorAvailableTime actor == stateRemainingTime s0
      ]
    movedActors =
      [ advanceStateByMoving g s0 actor dest
        | actor <- S.toList $ stateActors s0,
          actorAvailableTime actor == stateRemainingTime s0,
          dest <- M.keys (stateToOpen s0)
      ]

isTerminal :: State -> Bool
isTerminal s = stateRemainingTime s == 0

traceResult :: String -> (Int, [State]) -> (Int, [State])
traceResult lbl (s, states) = (s, traceList lbl states)

traceList :: Show a => String -> [a] -> [a]
traceList lbl = traceIt "ALL" . map (traceIt lbl)

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
