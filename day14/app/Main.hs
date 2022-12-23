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
import qualified Data.Map.Strict as Map
import Topograph (pairs)

main :: IO ()
main = do
  runInput "test.txt"
  runInput "input.txt"

runInput :: String -> IO ()
runInput fileName = do
  input <- readFile fileName
  putStrLn fileName
  print . day14 $ input

day14 :: String -> State
day14 = parseInput

data State = State
  { pointToObject :: Map.Map Point Char,
    xRange :: Range,
    yRange :: Range,
    fallCount :: Int
  }
  deriving (Eq)

instance Show State where
  show s = "\nx: " ++ show (xRange s) ++ "\ny: " ++ show (yRange s) ++ "\nfallen: " ++ show (fallCount s) ++ "\n" ++ showGrid s

showGrid :: State -> String
showGrid s =
  let ys = rangeValues (yRange s)
   in unlines . map (showGridLine s) $ ys

showGridLine :: State -> Int -> String
showGridLine s y =
  let xs = rangeValues (xRange s)
      m = pointToObject s
   in map (\x -> Map.findWithDefault ' ' (Point x y) m) xs

-- Parses input to produce the initial State
parseInput :: String -> State
parseInput text =
  let points = concatMap segmentPoints . concatMap parseLine . lines $ text
   in State
        { pointToObject = foldl addRock Map.empty points,
          xRange = collectRange . map getX $ points,
          yRange = collectRange . map getY $ points,
          fallCount = 0
        }

addRock :: Map.Map Point Char -> Point -> Map.Map Point Char
addRock m p = Map.insert p '#' m

data Point = Point Int Int deriving (Eq, Ord)

instance Show Point where
  show (Point x y) = "(" ++ show x ++ "," ++ show y ++ ")"

getX :: Point -> Int
getX (Point x _) = x

getY :: Point -> Int
getY (Point _ y) = y

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
