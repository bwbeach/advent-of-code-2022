module Main where

import Algorithm.Search (aStar)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Set as S
import Linear (V2 (..))

main :: IO ()
main = do
  runWithFile "test.txt"

runWithFile :: String -> IO ()
runWithFile fileName = do
  input <- readFile fileName
  let problem = parseInput input
  let steps = part1 problem
  putStrLn $ formatSequence steps
  print (length steps)
  putStrLn ""

formatSequence :: [Problem] -> [Char]
formatSequence steps =
  concat $ zipWith formatOne [1 ..] steps
  where
    formatOne :: Int -> Problem -> String
    formatOne i p = show i ++ ":\n" ++ formatProblem p ++ "\n"

part1 :: Problem -> [Problem]
part1 problem =
  states
  where
    (_, states) = fromJust $ aStar successors transitionCost remainingCost isSolution problem

-- | A position is a (V2 x y)
type Pos = V2 Int

-- | A direction of movement is a vector
type Dir = V2 Int

-- | The input problem is (expeditionPosition, width, height, blizzards)
type Problem = (Pos, Int, Int, [Blizzard])

-- | Each blizzard has a position and a direction
type Blizzard = (Pos, Dir)

-- | Convert input text into a Problem
parseInput :: String -> Problem
parseInput text =
  (initialPos, width, height, blizzards)
  where
    initialPos = V2 1 0
    rows = lines text
    width = length . head $ rows
    height = length rows

    blizzards = concat $ zipWith blizzardsInRow [0 ..] rows

    blizzardsInRow y chars = concat $ zipWith (blizzardsInChar y) [0 ..] chars

    blizzardsInChar y x c
      | c == '.' = []
      | c == '#' = []
      | otherwise = [(V2 x y, blizzardDir c)]

    blizzardDir c
      | c == '<' = V2 (-1) 0
      | c == '>' = V2 1 0
      | c == '^' = V2 0 (-1)
      | c == 'v' = V2 0 1
      | otherwise = error ("bad blizzard char: " ++ [c])

-- | Move all blizzards one step
moveBlizzards :: Problem -> Problem
moveBlizzards (pos, width, height, blizzards) =
  (pos, width, height, map moveBlizzard blizzards)
  where
    moveBlizzard (p, d) = (wrapPos (p + d), d)
    wrapPos (V2 x y) = V2 (wrap x width) (wrap y height)
    wrap n limit
      | n == 0 = limit - 2
      | n == limit - 1 = 1
      | otherwise = n

-- | Find all of the places the expedition could move
--
-- Assumes that blizzards have already moved, and can be used to see if a
-- new location is OK.
nextLocations :: Problem -> [Pos]
nextLocations (pos, width, height, blizzards) =
  filter isOk candidates
  where
    -- The possible destinations
    candidates = map (+ pos) moveChoices

    -- The five choices are to stay put, or move in one of the four directions
    moveChoices =
      [ V2 0 0,
        V2 1 0,
        V2 (-1) 0,
        V2 0 1,
        V2 0 (-1)
      ]

    -- Is this place OK to move to?
    isOk p = isOnBoard p && (not . isBlizzard) p

    -- Is this location on the board?
    isOnBoard (V2 x y) =
      0 < x
        && x < width - 1
        && (0 < y || (y == 0 && x == 1))
        && (y < height - 1 || (y == height - 1 && x == width - 2))

    -- Is there a blizzard at this location?
    isBlizzard p = p `elem` map fst blizzards

-- | Successors to a problem when searching
successors :: Problem -> [Problem]
successors problem =
  [ (nextPos, width, height, blizzards)
    | let (_, width, height, blizzards) = movedBlizzards,
      nextPos <- nextLocations movedBlizzards
  ]
  where
    movedBlizzards = moveBlizzards problem

-- | Cost to go from one state to another
--
-- Every move has the same cost.
transitionCost :: Problem -> Problem -> Int
transitionCost _ _ = 1

-- | Our estimate of the remaining cost from a position is the manhattan distance to the goal
remainingCost :: Problem -> Int
remainingCost p = manhattan (problemPosition p) (goalPosition p)

-- | Is this state a solution to the search
isSolution :: Problem -> Bool
isSolution p =
  problemPosition p == goalPosition p

-- | The current position of the expedition in a Problem
problemPosition :: Problem -> Pos
problemPosition (p, _, _, _) = p

-- | For a given problem, where are we trying to get to?
goalPosition :: Problem -> Pos
goalPosition (_, width, height, _) = V2 (width - 2) (height - 1)

-- | Manhattan distance between two positions
manhattan :: Pos -> Pos -> Int
manhattan (V2 x0 y0) (V2 x1 y1) = abs (x0 - x1) + abs (y0 - y1)

-- | Formats a state, so we can print it for testing
formatProblem :: Problem -> String
formatProblem problem@(pos, width, height, blizzards) =
  unlines rows
  where
    rows = map formatRow [0 .. height - 1]

    formatRow y = map (formatCell y) [0 .. width - 1]

    formatCell y x
      | V2 x y == pos = 'E'
      | isBorder x y = '#'
      | M.member (V2 x y) posToBlizzards = formatBlizzard (V2 x y)
      | otherwise = '.'

    formatBlizzard p =
      if S.size directions == 1
        then blizzardDirectionChar (head . S.toList $ directions)
        else head . show . S.size $ directions
      where
        directions = fromJust . M.lookup p $ posToBlizzards

    blizzardDirectionChar d =
      case d of
        V2 0 1 -> 'v'
        V2 0 (-1) -> '^'
        V2 1 0 -> '>'
        V2 (-1) 0 -> '<'
        _ -> error ("bad direction: " ++ show d)

    isBorder x y =
      (x == 0 || x == width - 1 || y == 0 || y == height - 1)
        && (V2 x y /= V2 1 0)
        && (V2 x y /= goal)

    goal = goalPosition problem

    -- map from position to the directions of blizzards there
    posToBlizzards = mapToSets blizzards

-- | Build a map whose values are sets.
mapToSets :: (Ord k, Ord v) => [(k, v)] -> M.Map k (S.Set v)
mapToSets =
  foldl addOne M.empty
  where
    addOne m (k, v) = updateMap S.empty k (S.insert v) m

-- | Update the value stored in a map.
updateMap ::
  Ord k =>
  v -> -- default value
  k -> -- key whose value will be updated
  (v -> v) -> -- function to update the value
  M.Map k v -> -- original map
  M.Map k v -- updated map
updateMap d k f m =
  M.insert k newValue m
  where
    oldValue = fromMaybe d (M.lookup k m)
    newValue = f oldValue
