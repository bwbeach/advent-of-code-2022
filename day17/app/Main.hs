module Main where

import MyLib (parseRocks, showRock)

main :: IO ()
main = do
  runInput "test.txt"

runInput :: String -> IO ()
runInput fileName = do
  rockText <- readFile "rocks.txt"
  let rocks = parseRocks rockText
  print rocks
  putStr . concatMap (("\n" ++) . showRock) $ rocks
