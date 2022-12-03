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
day01 n = sum . take n . reverse . sort . map sumStrings . splitOn [""] . endBy "\n"

sumStrings :: [String] -> Int
sumStrings = sum . map read
