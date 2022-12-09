{-
A Dock models the three stacks of crates on the dock, which the crane
can rearrange by moving crates from one stack to another.
-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-incomplete-uni-patterns #-}

module Dock
  ( Dock,
    empty,
    parse,
    move,
    tops,
  )
where

import qualified Data.IntMap.Strict as M
import qualified Data.List as L
import Debug.Trace

newtype Dock = Dock (M.IntMap [Char])

instance Show Dock where
  show (Dock d) =
    let tallest = maximum $ map length (M.elems d)
        extended = M.map (extend tallest) d
        zipped = L.transpose [getStack 1 extended, getStack 2 extended, getStack 3 extended]
        levels = map formatLevel zipped
     in unlines $ levels ++ [" 1   2   3 "]

tops :: Dock -> String
tops (Dock m) =
  [ first (getStack 1 m),
    first (getStack 2 m),
    first (getStack 3 m)
  ]

first :: String -> Char
first (c : _) = c

formatLevel :: String -> String
formatLevel crates = unwords $ map formatCrate crates

formatCrate :: Char -> String
formatCrate ' ' = "   "
formatCrate c = "[" ++ [c] ++ "]"

extend :: Int -> [Char] -> [Char]
extend n items = replicate (n - length items) ' ' ++ items

{-
   Input looks like:

           [D]
       [N] [C]
       [Z] [M] [P]
        1   2   3

-}

parse :: [String] -> Dock
parse rows =
  let crateLines = dropLast rows
   in Dock
        ( M.fromList
            [ (1, extractColumn 1 crateLines),
              (2, extractColumn 5 crateLines),
              (3, extractColumn 9 crateLines)
            ]
        )

extractColumn :: Int -> [String] -> [Char]
extractColumn col = filter (/= ' ') . map (!! col)

move :: Dock -> (Int, Int, Int) -> Dock
move start (count, from, to) =
  let Dock m0 = start
      src = getStack from m0
      items = take count src
      m1 = M.insert from (drop count src) m0
      dst = getStack to m1
      m2 = M.insert to (reverse items ++ dst) m1
   in trace (show (count, from, to) ++ "\n" ++ show (Dock m2) ++ "\n") Dock m2

dropLast :: [a] -> [a]
dropLast [_] = []
dropLast (a : as) = a : dropLast as

getStack :: Int -> M.IntMap [Char] -> [Char]
getStack = M.findWithDefault []

empty :: Dock
empty = Dock M.empty