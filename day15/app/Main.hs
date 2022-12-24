{-
Advent of Code 2022, Day 15
-}

module Main where

import Data.Char (isDigit)
import Data.List (sort)
import Data.Maybe (mapMaybe)
import Linear (V2 (..))

main :: IO ()
main = do
  runInput "test.txt" 10 20
  runInput "input.txt" 2000000 4000000

runInput :: String -> Int -> Int -> IO ()
runInput fileName row mapSize = do
  input <- readFile fileName
  putStrLn fileName
  print . day15a row $ input
  print . day15b mapSize $ input

day15a :: Int -> String -> Int
day15a r text =
  let sensors = parseInput text
      covered = makeIntSet (mapMaybe (sensorRangeAtRow r) sensors)
      otherLabels = makeIntSet . map (\p -> let n = pointX p in Range n n) . filter (\p -> pointY p == r) . concatMap sensorPoints $ sensors
      cannotBe = difference covered otherLabels
   in size cannotBe

day15b :: Int -> String -> [Int]
day15b mapSize text =
  let sensors = parseInput text
      allColumns = makeIntSet [Range 0 mapSize]
      excludedInRow r = makeIntSet (mapMaybe (sensorRangeAtRow r) sensors)
      couldBeInRow r = difference allColumns (excludedInRow r)
      pointsInRow r = map (`V2` r) (elements (couldBeInRow r))
      candidates = concatMap pointsInRow [0 .. mapSize]
   in map frequency candidates

frequency :: Num a => V2 a -> a
frequency (V2 x y) = x * 4000000 + y

sensorRangeAtRow :: Int -> Sensor -> Maybe Range
sensorRangeAtRow y s =
  let sensorToBeacon = manhattan (sensorPos s) (beaconPos s)
      sensorX = pointX (sensorPos s)
      sensorY = pointY (sensorPos s)
      sensorToRow = abs (y - sensorY)
      radius = sensorToBeacon - sensorToRow
   in if radius < 0
        then Nothing
        else Just (Range (sensorX - radius) (sensorX + radius))

{-
Point -- names a location on the grid
-}

type Point = V2 Int

pointX :: Point -> Int
pointX (V2 x _) = x

pointY :: Point -> Int
pointY (V2 _ y) = y

-- Return the Manhattan distance between two points
manhattan :: Point -> Point -> Int
manhattan (V2 x1 y1) (V2 x2 y2) = abs (x1 - x2) + abs (y1 - y2)

{-
Sensor -- holds the information reported by one sensor
-}

data Sensor = Sensor {sensorPos :: Point, beaconPos :: Point} deriving (Eq, Show)

-- Returns the two points this sensor shows on the grid: 'S' and 'B'
sensorPoints :: Sensor -> [Point]
sensorPoints s = [sensorPos s, beaconPos s]

-- Converts input text to a list of sensors
parseInput :: String -> [Sensor]
parseInput = map parseLine . lines

-- Converts one line to a Sensor
parseLine :: String -> Sensor
parseLine text =
  case map read . words . map nonNumberToSpace $ text of
    [x1, y1, x2, y2] -> Sensor (V2 x1 y1) (V2 x2 y2)
    _ -> error ("bad input line: " ++ text)

nonNumberToSpace :: Char -> Char
nonNumberToSpace c = if isNumberChar c then c else ' '

isNumberChar :: Char -> Bool
isNumberChar c = c == '-' || isDigit c

{-
Range -- an inclusive range of integers
-}

-- A range of integers
data Range = Range Int Int deriving (Eq, Ord, Show)

-- What are the elements in a range?
rangeElements :: Range -> [Int]
rangeElements (Range low high) = [low .. high]

-- How many numbers are in a range?
rangeCount :: Range -> Int
rangeCount (Range low high) = high - low + 1

-- Is the given value in the range?
rangeIncludes :: Range -> Int -> Bool
rangeIncludes (Range low high) n = low <= n && n <= high

-- Do two ranges overlap at all?
overlaps :: Range -> Range -> Bool
overlaps (Range l1 h1) (Range l2 h2) = l1 <= h2 && l2 <= h1

-- Combines a pair of ranges that overlap
combine :: Range -> Range -> Range
combine (Range l1 h1) (Range l2 h2) = Range (min l1 l2) (max h1 h2)

{-
IntSet - a set of integers, represented internall as a sequence of ranges.

The ranges are always stored as a sorted list of non-overalpping ranges.
-}

newtype IntSet = IntSet [Range] deriving (Show)

-- Creates a range set from a list of ranges; combines any ranges that overlap
makeIntSet :: [Range] -> IntSet
makeIntSet rs = IntSet (mergeRanges . sort $ rs)

-- Returns all of the elements in a set
elements :: IntSet -> [Int]
elements (IntSet rs) = concatMap rangeElements rs

-- Union of two sets
union :: IntSet -> IntSet -> IntSet
union (IntSet rs1) (IntSet rs2) = makeIntSet (rs1 ++ rs2)

-- Difference of two sets
difference :: IntSet -> IntSet -> IntSet
difference (IntSet rs1) (IntSet rs2) = IntSet (rangesDifference rs1 rs2)

-- Number of elements of an IntSet
size :: IntSet -> Int
size (IntSet rs) = sum . map rangeCount $ rs

-- Differencs between two lists of ranges
rangesDifference :: [Range] -> [Range] -> [Range]
rangesDifference rs [] = rs
rangesDifference [] _ = []
rangesDifference (r : rs) (x : xs)
  | overlaps r x = rangesDifference (rangeDifference r x ++ rs) (x : xs)
  | x < r = rangesDifference (r : rs) xs
  | otherwise = r : rangesDifference rs (x : xs)

-- Returns the list of ranges describing whats left after removing all points in the second range from the first
rangeDifference :: Range -> Range -> [Range]
rangeDifference (Range l1 h1) (Range l2 h2) =
  if overlaps (Range l1 h1) (Range l2 h2)
    then [Range l1 (l2 - 1) | l1 < l2] ++ [Range (h2 + 1) h1 | h2 < h1]
    else [Range l1 h1]

-- Combines ranges if they overlap.  Assumes input is sorted
mergeRanges :: [Range] -> [Range]
mergeRanges [] = []
mergeRanges [r] = [r]
mergeRanges (r0 : r1 : rs)
  | overlaps r0 r1 = mergeRanges (combine r0 r1 : rs)
  | otherwise = r0 : mergeRanges (r1 : rs)
