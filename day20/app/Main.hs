module Main (main) where

import Data.List (elemIndex)
import Data.Maybe (fromJust)

main :: IO ()
main = do
  runWithFile "test.txt"
  runWithFile "input.txt"

runWithFile :: FilePath -> IO ()
runWithFile fileName = do
  input <- readFile fileName
  putStrLn fileName
  let numbers = map read . lines $ input
  print . day20 $ numbers

day20 :: [Int] -> Int
day20 numbers = coords (mix numbers numbers)

-- | Add the coordinates
coords :: [Int] -> Int
coords ns =
  sum . map nthAfterZero $ [1000, 2000, 3000]
  where
    len = length ns
    indexOfZero = fromJust (elemIndex 0 ns)
    nthAfterZero i = ns !! ((i + indexOfZero) `mod` len)

-- | Perform a full mixing
mix :: [Int] -> [Int] -> [Int]
mix spec state =
  foldl (moveOne len) state spec
  where
    len = length spec

-- | Moves one number to a new place.
moveOne :: Int -> [Int] -> Int -> [Int]
moveOne len state n =
  before ++ [n] ++ after
  where
    -- we always move forward.  with a negative number, we wrap the
    -- circular list and move it forward.  The moving forward happens
    -- by jumping over all of the numbers *except* n, so after jumping
    -- over (len - 1), it's back to the same place.
    delta = n `mod` (len - 1)
    -- make a list, starting with the number after n, that is at least long enough
    afterN = drop 1 . dropWhile (/= n) $ (state ++ state)
    -- break the list at the point where n will be inserted
    (before, later) = splitAt delta afterN
    -- the 'later' part may be too long, so trim it down
    after = take (len - delta - 1) later
