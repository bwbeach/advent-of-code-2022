module Main where

import qualified Data.Char as Char
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

main :: IO ()
main = do
  runInput "test.txt"
  runInput "input.txt"

runInput :: String -> IO ()
runInput fileName = do
  input <- readFile fileName
  putStrLn fileName
  print . day12 $ input

day12 :: String -> Int
day12 input =
  let landscape = parseInput input
      distMap = distances landscape
   in distMap Map.! endPos landscape

data Point = Point Int Int deriving (Eq, Ord, Show)

neighbors :: Point -> [Point]
neighbors (Point x y) =
  [ Point (x - 1) y,
    Point (x + 1) y,
    Point x (y - 1),
    Point x (y + 1)
  ]

data Landscape = Landscape
  { startPos :: Point,
    endPos :: Point,
    posMap :: Map.Map Point Char
  }
  deriving (Show)

-- Returns a map of location to number of moves to get there
distances :: Landscape -> Map.Map Point Int
distances landscape =
  let mapSoFar = Map.fromList [(startPos landscape, 0)]
   in distancesHelper landscape mapSoFar [startPos landscape] 1

distancesHelper :: Landscape -> Map.Map Point Int -> [Point] -> Int -> Map.Map Point Int
distancesHelper landscape mapSoFar toDo dist =
  let candidates0 = concatMap (next landscape) toDo
      inLandscape p = Map.member p (posMap landscape)
      candidates1 = filter inLandscape candidates0
      notInMapSoFar p = not (Map.member p mapSoFar)
      candidates2 = filter notInMapSoFar candidates1
      candidates3 = Set.toList (Set.fromList candidates2)
   in if null candidates3
        then mapSoFar
        else
          let updatedMap = foldl (\m p -> Map.insert p dist m) mapSoFar candidates3
           in distancesHelper landscape updatedMap candidates3 (dist + 1)

-- Where can you move next, given a landscape
next :: Landscape -> Point -> [Point]
next landscape start =
  filter (canMove landscape start) (neighbors start)

-- True if you can move from point A to point B
canMove :: Landscape -> Point -> Point -> Bool
canMove landscape a b =
  Map.member b (posMap landscape)
    && let ca = getElevation landscape a
           cb = getElevation landscape b
        in Char.ord cb <= Char.ord ca + 1

-- Returns the Char representing the elevation at a point in the landscape
getElevation :: Landscape -> Point -> Char
getElevation landscape point =
  let c = posMap landscape Map.! point
   in if c == 'S' then 'a' else if c == 'E' then 'z' else c

parseInput :: String -> Landscape
parseInput text =
  let m = Map.fromList . concat . zipWith parseLine [0 ..] . lines $ text
   in Landscape
        { startPos = keyForValue 'S' m,
          endPos = keyForValue 'E' m,
          posMap = m
        }

parseLine :: Int -> String -> [(Point, Char)]
parseLine y = zipWith (\x c -> (Point x y, c)) [0 ..]

keyForValue :: Eq a => Show k => a -> Map.Map k a -> k
keyForValue a = oneAndOnly . map fst . filter (\(_, v) -> v == a) . Map.assocs

oneAndOnly :: [a] -> a
oneAndOnly [a] = a
oneAndOnly _ = error "not exactly one item in list"
