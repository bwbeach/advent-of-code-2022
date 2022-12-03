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
  print . day04 $ input

day04 :: String -> Int
day04 = length . endBy "\n"
