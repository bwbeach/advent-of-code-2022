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
  print . day02 $ input

day02 :: String -> Int
day02 = sum . map scoreLine . endBy "\n"

scoreLine :: String -> Int
scoreLine [theirCode, ' ', myCode] =
  let theirChoice = decode theirCode
      myChoice = decode myCode
   in scoreMyChoice myChoice + scoreOutcome myChoice theirChoice
scoreLine _ = -100000000

scoreOutcome :: String -> String -> Int
scoreOutcome myChoice theirChoice
  | isWin myChoice theirChoice = 6
  | isWin theirChoice myChoice = 0
  | otherwise = 3

scoreMyChoice :: String -> Int
scoreMyChoice "rock" = 1
scoreMyChoice "paper" = 2
scoreMyChoice "scissors" = 3
scoreMyChoice _ = -100000000

decode :: Char -> String
decode 'A' = "rock"
decode 'B' = "paper"
decode 'C' = "scissors"
decode 'X' = "rock"
decode 'Y' = "paper"
decode 'Z' = "scissors"
decode _ = "unknown"

isWin :: String -> String -> Bool
isWin "rock" "scissors" = True
isWin "paper" "rock" = True
isWin "scissors" "paper" = True
isWin _ _ = False