module Main (main) where

import Algorithm.Search
import Data.Char
import Data.List.Split
import qualified Data.Map.Strict as M
import Data.Maybe
import Debug.Trace
import MyLib

main :: IO ()
main = do
  runWithFile "test.txt"
  runWithFile "input.txt"

runWithFile :: String -> IO ()
runWithFile fileName = do
  putStrLn fileName
  input <- readFile fileName
  print . day19a $ input
  print . day19b $ input

day19a :: String -> Int
day19a = sum . map runOne . parseInput

day19b :: String -> Int
day19b = product . map (maxGeodes 32 . snd) . atMost 3 . parseInput

atMost :: Int -> [a] -> [a]
atMost 0 _ = []
atMost _ [] = []
atMost n (a : as) = a : atMost (n - 1) as

runOne :: (Int, Recipe) -> Int
runOne (blueprintNumber, recipe) =
  traceIt ("AAA " ++ show blueprintNumber) $ blueprintNumber * maxGeodes 24 recipe

maxGeodes :: Int -> Recipe -> Int
maxGeodes maxTime recipe =
  traceIt ("BBB " ++ show maxTime) $ nodeHowMany best (Res Geode)
  where
    best = last . snd . fromJust $ solution
    solution = aStar (nodeSuccessors recipe) edgeCost nodeRemainingCost isLeaf initialState
    isLeaf n = nodeTimeLeft n == 0
    nodeRemainingCost = const 0
    edgeCost n0 n1 = bestScore n0 - bestScore n1
    bestScore = bestPossibleScore recipe
    initialState = Node {nodeCounts = M.fromList [(Robot Ore, 1)], nodeTimeLeft = maxTime}

parseInput :: String -> [(Int, Recipe)]
parseInput = map parseRecipe . startBy "Blueprint"

startBy :: [Char] -> [Char] -> [[Char]]
startBy = split . dropInitBlank . dropDelims . onSublist

parseRecipe :: String -> (Int, Recipe)
parseRecipe text =
  case splitOn ":" text of
    [a, b] ->
      ( read . head . words $ a,
        makeRecipe . M.fromList . map parseElem . startBy "Each" . dropWhile isSpace . replaceChar '\n' ' ' $ b
      )
    _ -> error ("bad recipe: " ++ text)

parseElem :: String -> (Thing, [(Int, Thing)])
parseElem s =
  (Robot target, inputs)
  where
    s' = replaceChar '.' ' ' s
    ws = words s'
    target = read (capitalize (head ws))
    inputs = parseInputs (drop 3 ws)

parseInputs :: [String] -> [(Int, Thing)]
parseInputs = map parseOneInput . splitOn ["and"]

parseOneInput :: [String] -> (Int, Thing)
parseOneInput ws =
  (read (head ws), (Res . read . capitalize) (ws !! 1))

replaceChar :: Eq b => b -> b -> [b] -> [b]
replaceChar c1 c2 =
  map (\c -> if c == c1 then c2 else c)

capitalize :: [Char] -> [Char]
capitalize (c : cs) = toUpper c : cs
capitalize [] = []

traceIt :: Show a => [Char] -> a -> a
traceIt lbl x = trace (lbl ++ " " ++ show x) x