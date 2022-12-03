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
  print . day02 chooser1 $ input
  print . day02 chooser2 $ input

day02 :: (String -> Char -> String) -> String -> Int
day02 chooser = sum . map (scoreLine chooser) . endBy "\n"

chooser1 :: String -> Char -> String
chooser1 _ = decode

chooser2 :: String -> Char -> String
chooser2 theirChoice myCode =
  let outcome = decodeOutcome myCode
   in if scoreOutcome "rock" theirChoice == outcome
        then "rock"
        else
          if scoreOutcome "paper" theirChoice == outcome
            then "paper"
            else "scissors"

decodeOutcome :: Char -> Int
decodeOutcome 'X' = 0
decodeOutcome 'Y' = 3
decodeOutcome 'Z' = 6
decodeOutcome _ = -1

scoreLine :: (String -> Char -> String) -> String -> Int
scoreLine chooser [theirCode, ' ', myCode] =
  let theirChoice = decode theirCode
      myChoice = chooser theirChoice myCode
   in scoreMyChoice myChoice + scoreOutcome myChoice theirChoice
scoreLine _ _ = -100000000

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