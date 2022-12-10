{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Main where

main :: IO ()
main = do
  runInput "test.txt"
  runInput "input.txt"

runInput :: String -> IO ()
runInput fileName = do
  input <- readFile fileName
  putStrLn fileName
  print . day06 4 $ input
  print . day06 14 $ input

day06 :: Int -> String -> Int
day06 len codes =
  if allDifferent . take len $ codes
    then len
    else 1 + day06 len (butFirst codes)

butFirst :: [a] -> [a]
butFirst (_ : rest) = rest

allDifferent :: Eq a => [a] -> Bool
allDifferent [] = True
allDifferent (x : xs) = (x `notElem` xs) && allDifferent xs
