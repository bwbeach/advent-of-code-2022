{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-incomplete-uni-patterns #-}

module Main where

import qualified Data.IntMap.Strict as IntMap
import Data.List (sort)
import Data.List.Split (splitOn)

main :: IO ()
main = do
  runInput "test.txt"
  runInput "input.txt"

runInput :: String -> IO ()
runInput fileName = do
  input <- readFile fileName
  putStrLn fileName
  print . day11 20 $ input

day11 :: Int -> String -> Int
day11 numRounds =
  product . take 2 . reverse . sort . map inspections . IntMap.elems . runNRounds numRounds . parseInput

type Flock = IntMap.IntMap Monkey

data Monkey = Monkey
  { number :: Int,
    items :: [Int],
    operation :: Int -> Int,
    test :: Int -> Bool,
    ifTrue :: Int,
    ifFalse :: Int,
    inspections :: Int
  }

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
      level2 = level1 `div` 3
      target = if test monkey level2 then ifTrue monkey else ifFalse monkey
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
      test = parseTest . words $ line3,
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

parseTest :: [String] -> (Int -> Bool)
parseTest ["Test", "divisible", "by", s] n = n `rem` read s == 0

removeCommasAndColons :: String -> String
removeCommasAndColons = filter (\c -> c /= ',' && c /= ':')
