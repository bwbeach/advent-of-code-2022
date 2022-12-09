{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-incomplete-uni-patterns #-}

module Main where

import Data.List.Split
import qualified Dock as D

-- Stack of crates, with the top crate first
type Stack = [Char]

-- Sequence of stacks of crates
type Stacks = [Stack]

-- Move N crates from location A to location B
type Move = (Int, Int, Int)

main :: IO ()
main = do
  runInput "test.txt"
  runInput "input.txt"

runInput :: String -> IO ()
runInput fileName = do
  input <- readFile fileName
  putStrLn fileName
  print . day05 $ input

day05 :: String -> String
day05 input =
  let (dock, moves) = parseInput input
      afterMove = foldl D.move dock moves
   in D.tops afterMove

parseInput :: String -> (D.Dock, [Move])
parseInput text =
  let [stacks, moves] = (splitOn [""] . endBy "\n") text
   in (D.parse stacks, map parseMove moves)

-- Input looks like: "move 1 from 2 to 1"
parseMove :: String -> Move
parseMove s =
  let items = words s
   in (read (items !! 1), read (items !! 3), read (items !! 5))

-- Applies a function to the nth item in a list, replacing that nth item
-- with the result of the function.
mutateNth :: Int -> (a -> a) -> [a] -> [a]
mutateNth 0 f (a : as) = f a : as
mutateNth n f (a : as) = a : mutateNth (n - 1) f as