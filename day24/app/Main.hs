{-
    Advent of Code 2022, Day 24

    My first attempt used aStar search (a best-first search), where the
    next possible states were computed by moving all of the blizzards, and
    seeing which of the five possible next locations didn't collide with
    a blizzard or a wall.  That worked fine for the sample problem, but
    ran out of memory on the real problem.  Also, recomputing the next states
    of the blizzards on each move was a lot of repeated computation.

    I made two observations to improve the second attempt:

    (1) The blizzards repeat in a not-too-long period.  You can precompute
        a list of blizzard states and loop through it as the search progresses.

    (2) The product of all of the blizzard states and all of the possible
        positions of the expedition is small enough to fit in memory.  So treating
        the problem as a shortest-path graph problem can eliminate duplicate
        search states.  The graph nodes are named with (position, blizzard index),
        and with a graph search, if the same node is encountered again, it will
        net be searched again.

    The length of the blizzard repeat loop in the real problem is 700.  The number
    of possible positions of the expedition is 3500.  The total number of nodes in
    the complete graph being search is 2,450,000.
-}

module Main (main) where

import Algorithm.Search (dijkstra)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import qualified Data.Set as S
import Linear (V2 (..))

-- | A position is a (V2 x y)
type Pos = V2 Int

-- | A direction of movement is a vector (V2 dx dy)
type Dir = V2 Int

-- | The input problem is (width, height, blizzards)
type Input = (Int, Int, [Blizzard])

-- | Each blizzard in the input has a position and a direction
type Blizzard = (Pos, Dir)

-- | After pre-processing the input. the blizzards are a "loop".
--
-- For quick access to numbered entries, it's stored as a map from index-in-loop
-- to the set of positions where blizzards are.  After preprocessing, we no longer
-- need directions for blizzards; we just need to know where they are when checking
-- if the expedition can occupy a position.
type BlizzardLoop = M.Map Int (S.Set Pos)

-- | After pre-processing, the problem is (width, height, blizzardLoop)
type Problem = (Int, Int, BlizzardLoop)

main :: IO ()
main = do
  showStepsWithFile "test.txt"
  runWithFile "test.txt"
  runWithFile "input.txt"

-- | For testing, show the sequence of steps on the test problem.
showStepsWithFile :: String -> IO ()
showStepsWithFile fileName =
  do
    input <- readFile fileName
    let problem@(w, h, _) = parseProblem input
    putStrLn . concatMap (showStep problem) $ findPath problem (V2 1 0, 0) (V2 (w - 2) (h - 1))

-- | Format the grid at a given position/time.
showStep :: Problem -> (Pos, Int) -> [Char]
showStep (width, height, blizzardLoop) (p, t) =
  show t ++ ":\n" ++ unlines (map showLine [0 .. height - 1]) ++ "\n"
  where
    showLine y = map (showCell y) [0 .. width - 1]

    showCell y x
      | V2 x y == p = 'E'
      | x == 0 = '#'
      | x == width - 1 = '#'
      | y == 0 = if x == 1 then '.' else '#'
      | y == height - 1 = if x == width - 2 then '.' else '#'
      | otherwise = if isBlizzard (V2 x y) then '*' else '.'

    isBlizzard p' = S.member p' blizzardPositions

    blizzardPositions = fromJust (M.lookup loopIndex blizzardLoop)

    loopIndex = t `mod` M.size blizzardLoop

-- | Run parts 1 and 2 on the given file
runWithFile :: String -> IO ()
runWithFile fileName = do
  putStrLn fileName
  input <- readFile fileName
  let problem = parseProblem input
  print $ part1 problem
  print $ part2 problem

part1 :: Problem -> Int
part1 problem@(width, height, _) =
  length $ findPath problem initial target
  where
    start = V2 1 0
    target = V2 (width - 2) (height - 1)
    initial = (start, 0)

part2 :: Problem -> Int
part2 problem@(width, height, _) =
  -- build the path from start to target, back to start, and back to target.
  -- the result has the starting point as the first element, which doesn't
  -- count as a step.
  length . drop 1 $ foldl addToPath [initial] [target, start, target]
  where
    start = V2 1 0
    target = V2 (width - 2) (height - 1)
    initial = (start, 0)
    -- extend a path by adding steps to another destination
    addToPath pathSoFar dest =
      pathSoFar ++ findPath problem (last pathSoFar) dest

-- | Given a Problem, find the shortest path from an initial (position, loopIndex) to the target position.
--
-- Returns the sequence of (pos, index) states, ending with the target position.
findPath :: Problem -> (Pos, Int) -> Pos -> [(Pos, Int)]
findPath (width, height, blizzardLoop) initial target =
  path
  where
    (_, path) = fromJust $ dijkstra neighbors transCost isGoal initial

    -- the length of the loop
    loopLength = M.size blizzardLoop

    -- where the expedition can go next
    neighbors (p, li) =
      [ (p', li')
        | d <- moveChoices,
          let p' = p + d, -- position after moving
          isOnBoard p', -- only move to locations on the board
          not (S.member p' blizzardPositions) -- do not move where a blizzard is
      ]
      where
        li' = (li + 1) `mod` loopLength
        blizzardPositions = fromJust $ M.lookup li' blizzardLoop

    -- The five choices are to stay put, or move in one of the four directions
    moveChoices =
      [ V2 0 0,
        V2 1 0,
        V2 (-1) 0,
        V2 0 1,
        V2 0 (-1)
      ]

    -- All moves have the same cost
    transCost _ _ = 1 :: Int

    -- Is a state the goal?  Only position matters, not the loop index.
    isGoal (p, _) = p == target

    -- Is this location on the board?
    isOnBoard (V2 x y) =
      0 < x
        && x < width - 1
        && (0 < y || (y == 0 && x == 1))
        && (y < height - 1 || (y == height - 1 && x == width - 2))

-- | Parse input, and preprocess the blizzards
parseProblem :: String -> Problem
parseProblem text =
  (w, h, loop)
  where
    (w, h, blizzards) = parseInput text
    loop = makeBlizzardLoop w h blizzards

-- | Convert input text into an Input
parseInput :: String -> Input
parseInput text =
  (width, height, blizzards)
  where
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

-- | Start with initial blizzards, and generate the loop of sets of positions
--
-- The pattern of blizzards will repeat.  Detection of the repeats requires matching
-- direction as well as position.  But once you have the loop, each set of blizzards
-- can be reduced to just a position; it doesn't matter if there are multiple blizzards
-- at the same position.
--
-- The result is a map from zero-based index into the loop to a set of positions
-- where there are blizzards.
makeBlizzardLoop :: Int -> Int -> [Blizzard] -> BlizzardLoop
makeBlizzardLoop width height initialBlizzards =
  M.fromList . zip [0 ..] . map blizzardToPosSet $ blizzardLoop
  where
    -- Return the list of blizzards just until it repeats.
    blizzardLoop = takeUntilRepeat (iterate moveAll initialBlizzards)

    -- Move all of the blizzards one step, wrapping at edges.
    moveAll = map moveOne

    -- Move one blizzard one step
    moveOne (p, d) = (wrapPos (p + d), d)

    -- Wrap a position if it's gone past the edge
    wrapPos (V2 x y) = V2 (wrap x width) (wrap y height)
    wrap n limit
      | n == 0 = limit - 2
      | n == limit - 1 = 1
      | otherwise = n

    -- Convert a blizzard to a set of positions.
    blizzardToPosSet = S.fromList . map fst

-- | Given a list where the first element repeats, return everything up to (not icluding) the first repeat
takeUntilRepeat :: Eq a => [a] -> [a]
takeUntilRepeat [] = []
takeUntilRepeat (x : xs) = x : takeWhile (/= x) xs
