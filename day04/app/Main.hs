{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Main where

import Data.List.Split

main :: IO ()
main = do
  runInput "test.txt"
  runInput "input.txt"

runInput :: String -> IO ()
runInput fileName = do
  input <- readFile fileName
  putStrLn fileName
  print . day04 fullyContainsEitherWay $ input
  print . day04 overlapsAtAll $ input

day04 :: (((Int, Int), (Int, Int)) -> Bool) -> String -> Int
day04 f = length . filter f . map parseLine . endBy "\n"

parseLine :: String -> ((Int, Int), (Int, Int))
parseLine = listToTuple . fmap parseRange . splitOn ","

parseRange :: String -> (Int, Int)
parseRange = listToTuple . fmap read . splitOn "-"

listToTuple :: [a] -> (a, a)
listToTuple [x, y] = (x, y)

overlapsAtAll :: ((Int, Int), (Int, Int)) -> Bool
overlapsAtAll ((a, b), (c, d)) = c <= b && a <= d

fullyContainsEitherWay :: ((Int, Int), (Int, Int)) -> Bool
fullyContainsEitherWay (a, b) = fullyContains a b || fullyContains b a

fullyContains :: (Int, Int) -> (Int, Int) -> Bool
fullyContains (a, b) (c, d) = a <= c && d <= b