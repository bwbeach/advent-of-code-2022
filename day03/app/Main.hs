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
  print . day03a $ input
  print . day03b $ input

day03a :: String -> Int
day03a = sum . map (priority . commonItem) . endBy "\n"

day03b :: String -> Int
day03b = sum . map (priority . commonInAll) . groupIntoTriples . endBy "\n"

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

groupIntoTriples :: [String] -> [[String]]
groupIntoTriples [] = []
groupIntoTriples (a : b : c : rest) = [a, b, c] : groupIntoTriples rest

commonInAll :: [String] -> Char
commonInAll = uniqueElement . Set.elems . intersectAll . map Set.fromList

intersectAll :: [Set.Set Char] -> Set.Set Char
intersectAll [a] = a
intersectAll (a : b : rest) = intersectAll ((a `Set.intersection` b) : rest)