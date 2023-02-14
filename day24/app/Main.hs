{-
    Advent of Code 2022, Day 24
-}

module Main (main) where

import Algorithm.Search (dijkstra)
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S
import Linear (V2 (..))

-- | A position is a (V2 x y)
type Pos = V2 Int

-- | A direction of movement is a vector (V2 dx dy)
type Dir = V2 Int

-- | The input problem is (width, height, blizzards)
type Input = (Int, Int, [Blizzard])

-- | Each blizzard has a position and a direction
type Blizzard = (Pos, Dir)

-- | After pre-processing the input. the blizzards are a "loop".
--
-- The positions of the blizzards repeats within at most (width - 2) * (height - 2) steps.
-- The pre-processing builds a "loop" that is a map from index to a set of blizzard
-- positions.  The blizzards nolonger have directions, because after pre-processing
-- we don't need the directions any more; we just need to know where the blizzards are at each time.
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
  length $ findPath problem (V2 1 0, 0) (V2 (width - 2) (height - 1))

part2 :: Problem -> Int
part2 problem@(width, height, _) =
  snd $ foldl lastOfPath initial [target, start, target]
  where
    start = V2 1 0
    target = V2 (width - 2) (height - 1)
    initial = (start, 0)
    lastOfPath from to = last $ findPath problem from to

-- | Given a Problem, find the shortest path from an initial (position, time) to the target position.
findPath :: Problem -> (Pos, Int) -> Pos -> [(Pos, Int)]
findPath (width, height, blizzardLoop) initial target =
  path
  where
    (_, path) = fromJust $ dijkstra neighbors transCost isGoal initial

    -- the length of the loop
    loopLength = M.size blizzardLoop

    -- where the expedition can go next
    neighbors (p, t) =
      [ (p', t')
        | d <- moveChoices,
          let p' = p + d,
          isOnBoard p',
          not (S.member p' blizzardPositions)
      ]
      where
        t' = t + 1
        li' = t' `mod` loopLength
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

    -- Is a state the goal?
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
