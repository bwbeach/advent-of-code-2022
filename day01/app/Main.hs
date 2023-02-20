module Main where

import Data.Function ((&))
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
  print . day01 1 $ input
  print . day01 3 $ input

day01 :: Int -> String -> Int
day01 n text =
  text
    & lines
    & splitOn [""]
    & map sumStrings
    & sort
    & reverse
    & take n
    & sum

sumStrings :: [String] -> Int
sumStrings = sum . map read
