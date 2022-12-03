module Main where

import Data.List
import Data.List.Split

main :: IO ()
main = do
  runInput "test.txt"
  runInput "input.txt"

runInput :: String -> IO ()
runInput fileName = do
  input <- readFile fileName
  putStrLn fileName
  print . day01 1 $ input
  print . day01 3 $ input

day01 :: Int -> String -> Int
day01 n input = sum . take n . reverse . sort $ map sumStrings (splitOn [""] (endBy "\n" input))

readInt :: String -> Int
readInt s = read s :: Int

sumStrings :: [String] -> Int
sumStrings list = sum (map readInt list)
