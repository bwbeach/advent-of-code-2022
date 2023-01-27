module Main (main) where

import Data.Int (Int64)
import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Debug.Trace

main :: IO ()
main = do
  runWithFile "test.txt"
  runWithFile "input.txt"

runWithFile :: FilePath -> IO ()
runWithFile fileName = do
  input <- readFile fileName
  putStrLn fileName
  let numbers = map read . lines $ input
  print . day20a $ numbers
  print . day20b $ numbers

-- | For part 2 we need numbers bigger than ints, so define a type
type Value = Integer

-- | Part 1
day20a :: [Value] -> Value
day20a numbers = coords (mix numbers numbers)

-- | Part 2
day20b :: [Value] -> Value
day20b numbers =
  coords . nTimes 10 mixOnce $ bigNumbers
  where
    mixOnce = mix bigNumbers
    bigNumbers = map (* 811589153) numbers

-- | Apply a function to a value n times
nTimes :: Int -> (a -> a) -> a -> a
nTimes n f x = iterate f x !! n

-- | Add the coordinates
coords :: [Value] -> Value
coords ns =
  sum . map nthAfterZero $ [1000, 2000, 3000]
  where
    len = length ns
    indexOfZero = fromJust (elemIndex 0 ns)
    nthAfterZero i = ns !! ((i + indexOfZero) `mod` len)

-- | Perform a full mixing
mix :: [Value] -> [Value] -> [Value]
mix spec state =
  foldl (moveOne len) state spec
  where
    len = length spec

-- | Moves one number to a new place.
moveOne :: Int -> [Value] -> Value -> [Value]
moveOne len state n =
  before ++ [n] ++ after
  where
    -- we always move forward.  with a negative number, we wrap the
    -- circular list and move it forward.  The moving forward happens
    -- by jumping over all of the numbers *except* n, so after jumping
    -- over (len - 1), it's back to the same place.
    delta = fromEnum $ n `mod` (toEnum len - 1)
    -- make a list, starting with the number after n, that is at least long enough
    afterN = drop 1 . dropWhile (/= n) $ (state ++ state)
    -- break the list at the point where n will be inserted
    (before, later) = splitAt delta afterN
    -- the 'later' part may be too long, so trim it down
    after = take (len - delta - 1) later

traceIt :: Show a => [Char] -> a -> a
traceIt label x = trace (label ++ " " ++ show x) x