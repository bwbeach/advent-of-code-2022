{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-incomplete-uni-patterns #-}

module Main where

import qualified Data.IntMap.Strict as IntMap
import Data.List (sort)
import Data.List.Split (splitOn)
import Debug.Trace (trace)

main :: IO ()
main = do
  runInput "test.txt"
  runInput "input.txt"

runInput :: String -> IO ()
runInput fileName = do
  input <- readFile fileName
  putStrLn fileName
  print . day11 20 part1WorryReducerFactory $ input
  print . day11 10000 part2WorryReducerFactory $ input

day11 :: Int -> (Flock -> (Int -> Int)) -> String -> Int
day11 numRounds worryReducerFactory =
  product
    . take 2
    . reverse
    . sort
    . traceIt
    . map inspections
    . IntMap.elems
    . runNRounds numRounds
    . (\f -> IntMap.map (setWorryReducer (worryReducerFactory f)) f)
    . parseInput

traceIt :: Show a => a -> a
traceIt a = trace ("AAA " ++ show a) a

part1WorryReducerFactory :: Flock -> (Int -> Int)
part1WorryReducerFactory _ = (`div` 3)

part2WorryReducerFactory :: Flock -> (Int -> Int)
part2WorryReducerFactory flock n =
  n `rem` product (map testDivisibleBy (IntMap.elems flock))

type Flock = IntMap.IntMap Monkey

data Monkey = Monkey
  { number :: Int,
    items :: [Int],
    operation :: Int -> Int,
    worryReducer :: Int -> Int,
    testDivisibleBy :: Int,
    ifTrue :: Int,
    ifFalse :: Int,
    inspections :: Int
  }

setWorryReducer :: (Int -> Int) -> Monkey -> Monkey
setWorryReducer wr m = m {worryReducer = wr}

runNRounds :: Int -> Flock -> Flock
runNRounds n flock = iterate runRound flock !! n

runRound :: Flock -> Flock
runRound flock = foldl runMonkey flock (IntMap.keys flock)

runMonkey :: Flock -> Int -> Flock
runMonkey flock0 n =
  let monkey = flock0 IntMap.! n
      toProcess = items monkey
      flock1 = IntMap.insert n (monkey {items = [], inspections = inspections monkey + length toProcess}) flock0
   in foldl (runItem monkey) flock1 toProcess

runItem :: Monkey -> Flock -> Int -> Flock
runItem monkey flock level0 =
  let level1 = operation monkey level0
      level2 = worryReducer monkey level1
      target = if level2 `rem` testDivisibleBy monkey == 0 then ifTrue monkey else ifFalse monkey
      targetMonkey0 = flock IntMap.! target
      targetMonkey1 = targetMonkey0 {items = items targetMonkey0 ++ [level2]}
   in IntMap.insert target targetMonkey1 flock

parseInput :: String -> Flock
parseInput text =
  let monkeys = map parseMonkey . splitOn [""] . lines . removeCommasAndColons $ text
      pairs = map (\m -> (number m, m)) monkeys
   in IntMap.fromList pairs

parseMonkey :: [String] -> Monkey
parseMonkey [line0, line1, line2, line3, line4, line5] =
  Monkey
    { number = read . last . words $ line0,
      items = map read . drop 2 . words $ line1,
      operation = parseOperation . words $ line2,
      worryReducer = id, -- will be updated
      testDivisibleBy = parseTestDivisibleBy . words $ line3,
      ifTrue = read . last . words $ line4,
      ifFalse = read . last . words $ line5,
      inspections = 0
    }

parseOperation :: [String] -> (Int -> Int)
parseOperation ["Operation", "new", "=", "old", "+", "old"] = \x -> x * x
parseOperation ["Operation", "new", "=", "old", "*", "old"] = \x -> x * x
parseOperation ["Operation", "new", "=", "old", op, s] = parseOp op (read s)
parseOperation x = error ("Bad operation: " ++ show x)

parseOp :: String -> Int -> (Int -> Int)
parseOp "*" n x = x * n
parseOp "+" n x = x + n

parseTestDivisibleBy :: [String] -> Int
parseTestDivisibleBy ["Test", "divisible", "by", s] = read s

removeCommasAndColons :: String -> String
removeCommasAndColons = filter (\c -> c /= ',' && c /= ':')
