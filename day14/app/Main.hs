{-
Advent of Code 2022, Day 14

Grains of sand are falling into a cave with solid rock structures
here and there.

My plan is to make a function (Point -> State -> State) that
given a position of a grain of sand, and an old state, computes
the new state after that grain of sand comes to rest.

The state includes the positions of all of the rock structures ('#')
and grains of sand (o), the y position of the lowest rock structure,
and the number of grains of sand that have fallen off the bottom.

-}

module Main where

import Data.List.Split (splitOn)
import Debug.Trace
import Grid
import Topograph (pairs)

main :: IO ()
main = do
  runInput "test.txt"

-- runInput "input.txt"

runInput :: String -> IO ()
runInput fileName = do
  input <- readFile fileName
  putStrLn fileName
  print . day14 $ input

day14 :: String -> Int
day14 text =
  let s0 = parseInput text
      g0 = pointToObject s0
      bottomY = maxY (gridBounds g0)
      s1 = s0 {fallThreshold = Just (2 + bottomY)}
   in (\n -> n - 1) . length . traceLast . takeWhile stateNoneFallen . iterate addOneSand $ s1

traceLast :: Show a => [a] -> [a]
traceLast states = trace (show (last states)) states

sandDropPoint :: Point
sandDropPoint = Point 500 0

addOneSand :: State -> State
addOneSand s = dropOneSandFrom s sandDropPoint

dropOneSandFrom :: State -> Point -> State
dropOneSandFrom s p =
  if sandHasFallen s p
    then s {fallCount = fallCount s + 1}
    else case filter (stateEmptyAt s) (dropChoices p) of
      (p1 : _) -> dropOneSandFrom s p1
      [] -> s {pointToObject = insert p 'o' (pointToObject s)}

dropChoices :: Point -> [Point]
dropChoices (Point x y) =
  [ Point x (y + 1),
    Point (x - 1) (y + 1),
    Point (x + 1) (y + 1)
  ]

data State = State
  { pointToObject :: Grid Char,
    fallThreshold :: Maybe Int,
    fallCount :: Int
  }
  deriving (Eq)

instance Show State where
  show s = "fallen: " ++ show (fallCount s) ++ "\n" ++ showGrid s

stateEmptyAt :: State -> Point -> Bool
stateEmptyAt s p = not (member p (pointToObject s))

stateNoneFallen :: State -> Bool
stateNoneFallen s = fallCount s == 0

sandHasFallen :: State -> Point -> Bool
sandHasFallen s p =
  case fallThreshold s of
    Nothing -> False
    Just n -> n < pointY p

showGrid :: State -> String
showGrid s =
  let g = pointToObject s
      bounds = gridBounds g
      xs = [minX bounds .. maxX bounds]
      ys = [minY bounds .. maxY bounds]
   in unlines . map (showGridLine g xs) $ ys

showGridLine :: Grid Char -> [Int] -> Int -> [Char]
showGridLine g xs y =
  map (\x -> getWithDefault ' ' (Point x y) g) xs

-- Parses input to produce the initial State
parseInput :: String -> State
parseInput text =
  let points = concatMap segmentPoints . concatMap parseLine . lines $ text
   in State
        { pointToObject = foldl addRock empty points,
          fallThreshold = Nothing,
          fallCount = 0
        }

addRock :: Grid Char -> Point -> Grid Char
addRock g p = insert p '#' g

data Segment = Segment Point Point deriving (Eq, Ord)

instance Show Segment where
  show (Segment p1 p2) = show p1 ++ " -> " ++ show p2

-- Returns a list of points in a segment
segmentPoints :: Segment -> [Point]
segmentPoints (Segment p1 p2) =
  if p1 == p2
    then [p1]
    else p1 : segmentPoints (Segment (oneStep p1 p2) p2)

-- returns the first point, moved one step towards the second point
oneStep :: Point -> Point -> Point
oneStep (Point x1 y1) (Point x2 y2) =
  Point (oneStepOneAxis x1 x2) (oneStepOneAxis y1 y2)

oneStepOneAxis :: Int -> Int -> Int
oneStepOneAxis a b
  | a < b = a + 1
  | a == b = a
  | otherwise = a - 1

-- Holds the min and max values
data Range = Range
  { rangeMin :: Int,
    rangeMax :: Int
  }
  deriving (Eq, Show)

-- Finds the range of a sequence of values
collectRange :: [Int] -> Range
collectRange (n : ns) = foldl updateRange (Range n n) ns
collectRange [] = error "cannot build range from empty list"

-- Returns a new range that includes the given value
updateRange :: Range -> Int -> Range
updateRange (Range a b) n = Range (min a n) (max b n)

-- Returns all of the values in a range
rangeValues :: Range -> [Int]
rangeValues (Range a b) = [a .. b]

{-
Parses an input line, producing a list of points.

Input lines look like:
498,4 -> 498,6 -> 496,6
-}
parseLine :: String -> [Segment]
parseLine = map (uncurry Segment) . pairs . map parsePoint . splitOn " -> "

-- Parses a string like "523,136" into a Point
parsePoint :: String -> Point
parsePoint text =
  case map read (splitOn "," text) of
    [x, y] -> Point x y
    _ -> error ("bad point: " ++ text)
