{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Main where

import Data.Char
import Data.List.Split
import qualified Data.Set as Set

main :: IO ()
main = do
  runInput "test.txt"
  runInput "input.txt"

runInput :: String -> IO ()
runInput fileName = do
  input <- readFile fileName
  putStrLn fileName
  print . day03 $ input

day03 :: String -> Int
day03 = sum . map (priority . commonItem) . endBy "\n"

priority :: Char -> Int
priority c
  | 'a' <= c && c <= 'z' = 1 + ord c - ord 'a'
  | 'A' <= c && c <= 'Z' = 27 + ord c - ord 'A'

commonItem :: String -> Char
commonItem s =
  let len = length s
      half = len `div` 2
      first = Set.fromList (take half s)
      second = Set.fromList (drop half s)
      common = Set.intersection first second
   in uniqueElement (Set.elems common)

uniqueElement :: [Char] -> Char
uniqueElement [c] = c
