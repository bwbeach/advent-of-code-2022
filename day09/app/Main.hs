{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-incomplete-uni-patterns #-}

module Main where

import Data.Set as Set

main :: IO ()
main = do
  runInput "test.txt"
  runInput "test2.txt"
  runInput "input.txt"

runInput :: String -> IO ()
runInput fileName = do
  input <- readFile fileName
  putStrLn fileName
  print . day08 2 $ input
  print . day08 10 $ input

day08 :: Int -> String -> Int
day08 knotCount input =
  let dirs = parseInput input
      initial = replicate knotCount (Point 0 0)
      oneStep s0 d = let s1 = update s0 d in (last s1, s1)
      tailPoints = knitl oneStep initial dirs
   in Set.size . Set.fromList $ tailPoints

-- A point on a rectangular grid
data Point = Point Int Int deriving (Eq, Ord)

-- A sequence of points, starting with the head and ending with the tail
type State = [Point]

-- One of the directions a rope end can move
data Dir = L | R | U | D deriving (Eq, Ord, Read)

-- Reads the input text and translates to a sequence of Dirs by
-- replicating the direction on each line by the count that goes
-- with it.
parseInput :: String -> [Dir]
parseInput = concatMap parseLine . lines

parseLine :: String -> [Dir]
parseLine text =
  let [w0, w1] = words text
   in replicate (read w1) (read w0)

-- Updates a [H, ..., T] position by moving the H a given direction
-- and moving the Ts according to the rules.
update :: State -> Dir -> State
update (h0 : ts) dir =
  let h1 = move h0 dir
   in h1 : updateTails h1 ts

updateTails :: Point -> [Point] -> [Point]
updateTails _ [] = []
updateTails prev (t0 : ts) =
  let t1 = updateTail t0 prev
   in (t1 : updateTails t1 ts)

-- Moves a Point in the given direction
-- Positive x is to the right, positive y is down
move :: Point -> Dir -> Point
move (Point x y) R = Point (x + 1) y
move (Point x y) L = Point (x - 1) y
move (Point x y) D = Point x (y + 1)
move (Point x y) U = Point x (y - 1)

-- Updates the tail of a rope if it's too far from the head
updateTail :: Point -> Point -> Point
updateTail (Point xt yt) (Point xh yh)
  | withinOne xt xh && withinOne yt yh = Point xt yt
  | xt < xh - 1 && yt == yh = Point (xt + 1) yt
  | xh + 1 < xt && yt == yh = Point (xt - 1) yt
  | xt == xh && yt < yh - 1 = Point xt (yt + 1)
  | xt == xh && yh + 1 < yt = Point xt (yt - 1)
  | xt < xh && yt < yh = Point (xt + 1) (yt + 1)
  | xt > xh && yt > yh = Point (xt - 1) (yt - 1)
  | xt < xh = Point (xt + 1) (yt - 1)
  | otherwise = Point (xt - 1) (yt + 1)

withinOne :: Int -> Int -> Bool
withinOne a b = a - 1 <= b && b <= a + 1

-- Uses function f to map every element of a list, and weave a value from left to right.
--
-- Output of the function is a mapped value to include in the resulting list, and
-- a value to pass forward for the next element.
--
--          b0    b1    b2
--          |     |     |
--          v     v     v
--    a --> f --> f --> f
--          |     |     |
--          v     v     v
--          c0    c1    c2
--
knitl :: (a -> b -> (c, a)) -> a -> [b] -> [c]
knitl _ _ [] = []
knitl f a0 (b : bs) =
  let (c, a1) = f a0 b
   in (c : knitl f a1 bs)
