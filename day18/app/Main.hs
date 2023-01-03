module Main where

import Data.List.Split
import qualified Data.Set as S
import Linear.V3 (V3 (..))

main :: IO ()
main = do
  runInput "test.txt"
  runInput "input.txt"

runInput :: String -> IO ()
runInput fileName = do
  input <- readFile fileName
  putStrLn fileName
  let droplet = parseInput $ input
  print . sum . map (openFaces droplet) . S.toList $ droplet

type Point = V3 Int

-- | Returns the number of open faces of one unit cube
openFaces :: S.Set Point -> Point -> Int
openFaces droplet =
  length . filter (\p -> not (p `S.member` droplet)) . neighbors

-- | Returns all of the neighboring points of a point, but not the point itself.
neighbors :: Point -> [Point]
neighbors p = [p + d | d <- faces]

faces :: [Point]
faces =
  [ V3 (negate 1) 0 0,
    V3 0 (negate 1) 0,
    V3 0 0 (negate 1),
    V3 1 0 0,
    V3 0 1 0,
    V3 0 0 1
  ]

zero :: Point
zero = V3 0 0 0

parseInput :: String -> S.Set Point
parseInput = S.fromList . map parsePoint . lines

parsePoint :: String -> Point
parsePoint = pointFromList . map read . splitOn ","

pointFromList :: [Int] -> Point
pointFromList [x, y, z] = V3 x y z
pointFromList _ = error "bad point"