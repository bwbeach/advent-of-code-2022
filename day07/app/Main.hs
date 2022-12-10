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
  print . day07 $ input

day07 :: String -> Int
day07 = length . lines
