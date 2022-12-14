{-
Advent of Code 2022: Day 8
https://adventofcode.com/2022/day/8
-}

module Main where

import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromJust)
import qualified Data.Set as Set

main :: IO ()
main = do
  runInput "test.txt"
  runInput "input.txt"

runInput :: String -> IO ()
runInput fileName = do
  input <- readFile fileName
  putStrLn fileName
  print . day08 $ input

-- Finds, then counts, the positions of all visible trees in the forest.
--
-- Works by adding all of the visible trees on all sightlines
-- to a set to avoid duplicates.
day08 :: String -> Int
day08 input =
  let forest = parseForest input
      sightlines = allSightlines forest
   in Set.size . Set.fromList $ concatMap (visibleInSightline forest) sightlines

-- A Point names one location in the forest.
-- It's a point on a rectangular grid.  Upper left corner is (Point 0 0)
data Point = Point Int Int deriving (Eq, Ord, Show)

-- A Forest is a rectangular grid of tree heights.
-- The three items are the width, the height, and the mapping from location to tree height
data Forest = Forest Int Int (Map.Map Point Int)

-- A Sightline is a sequence of points in a straight line, starting
-- at the closest point in the forest, and moving away from the viewer.
type Sightline = [Point]

-- Parses input text into a Forest
parseForest :: String -> Forest
parseForest text =
  let rows = lines text
      width = length (head rows)
      height = length rows
      kvpairs = concat (zipWith parseRow [0 ..] rows)
   in Forest width height (Map.fromList kvpairs)

-- Turns one row of a forest into (k, v) pairs for the map
--
-- The input string is a list of one-character tree heights.
-- A list of X coordinates is zipped with the tree heights
-- to produce pairs (x, c), which are then turned into
-- (location, height).
parseRow :: Int -> String -> [(Point, Int)]
parseRow y = zipWith (\x c -> (Point x y, read [c])) [0 ..]

-- Displays a forest
instance Show Forest where
  show (Forest w h m) = unlines (map (\y -> formatRow w y m) (countUp h))

-- Displays one row in a forest
formatRow :: Int -> Int -> Map.Map Point Int -> String
formatRow w y m = concatMap (\x -> show (m Map.! Point x y)) (countUp w)

-- Returns a list of all lines of sight an observer can use to look at trees.
-- Each one is a sequence of points moving away from the observer.
allSightlines :: Forest -> [Sightline]
allSightlines (Forest w h _) =
  map (\y -> map (`Point` y) (countUp w)) (countUp h)
    ++ map (\y -> map (`Point` y) (countDown w)) (countUp h)
    ++ map (\x -> map (x `Point`) (countUp h)) (countUp w)
    ++ map (\x -> map (x `Point`) (countDown h)) (countUp w)

-- Numbers from 0 to n-1
countUp :: Int -> [Int]
countUp n = [0 .. (n - 1)]

-- Numbers from n-1 down to 0
countDown :: Int -> [Int]
countDown n = [n - 1, n - 2 .. 0]

-- Uses function f to map every element of a list, and weave a value from left to right.
--
-- Output of the function is a mapped value to include in the resulting list, and
-- a value to pass forward for the next element.
--
--          b0    b1    b2
--          |     |     |
--          v     v     v
--    a --> f --> f --> f
--          |     |     |
--          v     v     v
--          c0    c1    c2
--
knitl :: (a -> b -> (c, a)) -> a -> [b] -> [c]
knitl _ _ [] = []
knitl f a0 (b : bs) =
  let (c, a1) = f a0 b
   in (c : knitl f a1 bs)

-- Function to use with knit to see if a tree is visible.
--
-- Inputs are the forest, the height of the tallest previous
-- tree, and the position of the tree in question.
--
-- Outputs are a Point, if this tree is visible, and the
-- tallest height so far.
checkVisible :: Forest -> Int -> Point -> (Maybe Point, Int)
checkVisible forest h0 p =
  let h1 = getHeight forest p
   in (if h0 < h1 then Just p else Nothing, max h0 h1)

-- Returns the height of a tree in the forest
getHeight :: Forest -> Point -> Int
getHeight forest point =
  let (Forest _ _ m) = forest
   in fromJust (Map.lookup point m)

-- Returns all of the positions of visible trees along the
-- given sightline.
visibleInSightline :: Forest -> Sightline -> [Point]
visibleInSightline forest sightline =
  catMaybes (knitl (checkVisible forest) negativeOne sightline)

negativeOne :: Int
negativeOne = -1
