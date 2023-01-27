module Main (main) where

import Data.List (findIndex)
import Data.Maybe (fromJust)

main :: IO ()
main = do
  runWithFile "test.txt"
  runWithFile "input.txt"

runWithFile :: FilePath -> IO ()
runWithFile fileName = do
  input <- readFile fileName
  putStrLn fileName
  let values = map read . lines $ input
  let items = zip [1 ..] values
  print . day20a $ items
  print . day20b $ items

-- | For part 2 we need numbers bigger than ints, so define a type
type Value = Integer

-- There are duplicate numbers.  We want to label each one with its
-- original position, so we can move them in the right order.
type Item = (Int, Value)

itemValue :: Item -> Value
itemValue (_, v) = v

-- | Part 1
day20a :: [Item] -> Value
day20a items = coords (mix items items)

-- | Part 2
day20b :: [Item] -> Value
day20b items =
  coords . nTimes 10 mixOnce $ bigItems
  where
    mixOnce = mix bigItems
    makeItemBig (i, n) = (i, n * 811589153)
    bigItems = map makeItemBig items

-- | Apply a function to a value n times
nTimes :: Int -> (a -> a) -> a -> a
nTimes n f x = iterate f x !! n

-- | Add the coordinates
coords :: [Item] -> Value
coords ns =
  sum . map (itemValue . nthAfterZero) $ [1000, 2000, 3000]
  where
    len = length ns
    indexOfZero = fromJust (findIndex (\item -> itemValue item == 0) ns)
    nthAfterZero i = ns !! ((i + indexOfZero) `mod` len)

-- | Perform a full mixing
mix :: [Item] -> [Item] -> [Item]
mix spec state =
  foldl (moveOne len) state spec
  where
    len = length spec

-- | Moves one number to a new place.
moveOne :: Int -> [Item] -> Item -> [Item]
moveOne len state n =
  before ++ [n] ++ after
  where
    -- we always move forward.  with a negative number, we wrap the
    -- circular list and move it forward.  The moving forward happens
    -- by jumping over all of the numbers *except* n, so after jumping
    -- over (len - 1), it's back to the same place.
    delta = fromEnum $ itemValue n `mod` (toEnum len - 1)
    -- make a list, starting with the number after n, that is at least long enough
    afterN = drop 1 . dropWhile (/= n) $ (state ++ state)
    -- break the list at the point where n will be inserted
    (before, later) = splitAt delta afterN
    -- the 'later' part may be too long, so trim it down
    after = take (len - delta - 1) later
