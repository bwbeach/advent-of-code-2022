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

import qualified Data.Char as C
import qualified Data.IntMap.Strict as M
import qualified Data.List as L
import Data.Maybe (mapMaybe)

newtype Dock = Dock (M.IntMap [Char])

instance Show Dock where
  show (Dock d) =
    let tallest = maximum $ map length (M.elems d)
        extended = M.map (extend tallest) d
        zipped = L.transpose (M.elems extended)
        levels = map formatLevel zipped
        legend = L.intercalate "   " (map show (M.keys extended))
     in unlines $ levels ++ [" " ++ legend]

tops :: Dock -> String
tops (Dock m) = map first (M.elems m)

first :: [a] -> a
first (x : _) = x

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
  let (key : bottomToTop) = reverse rows
      addList = concatMap (makeAddList key) bottomToTop
   in foldl addToDock empty addList

addToDock :: Dock -> (Int, Char) -> Dock
addToDock (Dock m0) (stack, crate) =
  let before = getStack stack m0
      after = crate : before
      m1 = M.insert stack after m0
   in Dock m1

makeAddList :: String -> String -> [(Int, Char)]
makeAddList key crates = mapMaybe makeAdd (zip key crates)

makeAdd :: (Char, Char) -> Maybe (Int, Char)
makeAdd (key, crate) =
  if C.isDigit key && C.isLetter crate
    then Just (read [key], crate)
    else Nothing

move :: (String -> String) -> Dock -> (Int, Int, Int) -> Dock
move xform start (count, from, to) =
  let Dock m0 = start
      src = getStack from m0
      items = take count src
      m1 = M.insert from (drop count src) m0
      dst = getStack to m1
      m2 = M.insert to (xform items ++ dst) m1
   in Dock m2

getStack :: Int -> M.IntMap [Char] -> [Char]
getStack = M.findWithDefault []

empty :: Dock
empty = Dock M.empty