module Main where

import Data.List.Split
import qualified Data.Set as S
import Linear.V3 (V3 (..))
import MyLib

main :: IO ()
main = do
  runInput "test.txt"
  runInput "input.txt"

runInput :: String -> IO ()
runInput fileName = do
  input <- readFile fileName
  putStrLn fileName
  let droplet = parseInput $ input
  print . part1 $ droplet
  print . part2 $ droplet

part1 :: S.Set Point -> Int
part1 droplet = sum . map (openFaces droplet) . S.toList $ droplet

part2 :: S.Set Point -> Int
part2 droplet =
  sum . map (openToOutside outsideDroplet) . S.toList $ droplet
  where
    (minX, maxX) = minMax . map pointX . S.toList $ droplet
    (minY, maxY) = minMax . map pointY . S.toList $ droplet
    (minZ, maxZ) = minMax . map pointZ . S.toList $ droplet
    inBounds (V3 x y z) =
      minX - 1 <= x
        && x <= maxX + 1
        && minY - 1 <= y
        && y <= maxY + 1
        && minZ - 1 <= z
        && z <= maxZ + 1
    notInDroplet p = not (S.member p droplet)
    nextNodes = filter notInDroplet . filter inBounds . neighbors
    outsideDroplet = reachable nextNodes (V3 (minX - 1) (minY - 1) (minZ - 1))

minMax :: [Int] -> (Int, Int)
minMax ns = (minimum ns, maximum ns)

type Point = V3 Int

pointX :: V3 a -> a
pointX (V3 x _ _) = x

pointY :: V3 a -> a
pointY (V3 _ y _) = y

pointZ :: V3 a -> a
pointZ (V3 _ _ z) = z

-- | Returns the number of open faces of one unit cube
openFaces :: S.Set Point -> Point -> Int
openFaces droplet =
  length . filter (\p -> not (p `S.member` droplet)) . neighbors

openToOutside :: S.Set Point -> Point -> Int
openToOutside outsideDroplet =
  length . filter (`S.member` outsideDroplet) . neighbors

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