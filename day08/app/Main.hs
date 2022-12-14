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

day08 :: String -> Int
day08 input =
  let forest = parseForest input
      sightlines = allSightlines forest
   in Set.size . Set.fromList $ concatMap (visibleInSightline forest) sightlines

-- Names one location in the forest.  Upper left corner is (Point 0 0)
data Point = Point Int Int deriving (Eq, Ord, Show)

-- A rectangular grid of tree heights
data Forest = Forest Int Int (Map.Map Point Int)

-- A sequence of tree locations, from close to far
type Sightline = [Point]

-- Parses an input forest into a Forest
parseForest :: String -> Forest
parseForest text =
  let rows = lines text
      width = length (head rows)
      height = length rows
      kvpairs = concat (zipWith parseRow [0 ..] rows)
   in Forest width height (Map.fromList kvpairs)

-- Turns one row of a forest into (k, v) pairs for the map
parseRow :: Int -> String -> [(Point, Int)]
parseRow y = zipWith (\x c -> (Point x y, read [c])) [0 ..]

-- Displays a forest
instance Show Forest where
  show (Forest w h m) = unlines (map (\y -> formatRow w y m) [0 .. (h - 1)])

-- Displays one row in a forest
formatRow :: Int -> Int -> Map.Map Point Int -> String
formatRow w y m = concatMap (\x -> show (m Map.! Point x y)) [0 .. (w - 1)]

-- Returns a list of all lines of sight.
-- A line of sight is a list of coordinates looking out from a viewpoint
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

knitl :: (a -> b -> (c, a)) -> a -> [b] -> [c]
knitl _ _ [] = []
knitl f a0 (b : bs) =
  let (c, a1) = f a0 b
   in (c : knitl f a1 bs)

checkVisible :: Forest -> Int -> Point -> (Maybe Point, Int)
checkVisible forest h0 p =
  let h1 = getHeight forest p
   in (if h0 < h1 then Just p else Nothing, max h0 h1)

getHeight :: Forest -> Point -> Int
getHeight forest point =
  let (Forest _ _ m) = forest
   in fromJust (Map.lookup point m)

visibleInSightline :: Forest -> Sightline -> [Point]
visibleInSightline forest sightline =
  catMaybes (knitl (checkVisible forest) negativeOne sightline)

negativeOne :: Int
negativeOne = -1
