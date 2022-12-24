{-
Advent of Code 2022, Day 15
-}

module Main where

import Data.Char (isDigit)
import Data.List (sort)
import Data.Maybe (mapMaybe)
import Debug.Trace (trace)
import Linear (V2 (..))

main :: IO ()
main = do
  runInput "test.txt" 10
  runInput "input.txt" 2000000

runInput :: String -> Int -> IO ()
runInput fileName r = do
  input <- readFile fileName
  putStrLn fileName
  print . day15 r $ input

day15 :: Int -> String -> Int
day15 r text =
  let sensors = parseInput text
      ranges0 = mapMaybe (sensorRangeAtRow r) sensors
      toRemove = traceIt . map pointX . filter (\p -> pointY p == r) . concatMap sensorPoints $ sensors
      ranges1 = foldl removeFromRanges ranges0 toRemove
   in sum . map rangeCount . traceIt . mergeRanges . traceIt . sort $ ranges1

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

traceIt :: Show a => a -> a
traceIt x = trace (show x) x

{-
Range -- an inclusive range of integers
-}

-- A range of integers
data Range = Range Int Int deriving (Eq, Ord, Show)

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

-- Removes a number from every range given
removeFromRanges :: [Range] -> Int -> [Range]
removeFromRanges rs n = concatMap (`removeFromRange` n) rs

-- Removes a given value from a range
removeFromRange :: Range -> Int -> [Range]
removeFromRange r n = rangeDifference r (Range n n)

{-
RangeSet - a set of ranges, reduced to minimal form.

Minimal form is a sorted list of non-overlapping ranges.
-}

newtype IntSet = IntSet [Range] deriving (Show)

-- Creates a range set from a list of ranges; combines any ranges that overlap
makeIntSet :: [Range] -> IntSet
makeIntSet rs = IntSet (mergeRanges . sort $ rs)

-- Union of two sets
union :: IntSet -> IntSet -> IntSet
union (IntSet rs1) (IntSet rs2) = makeIntSet (rs1 ++ rs2)

-- Difference of two sets
difference :: IntSet -> IntSet -> IntSet
difference (IntSet rs1) (IntSet rs2) = IntSet (rangesDifference rs1 rs2)

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
