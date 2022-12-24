{-

A rectilinear grid of things.  Indexed by V2.

Each grid maintaints the min/max x/y bounds.  The bounds for
a grid are set whet the first item is added, and only grow
because no element is ever removed from a grid.

-}

module Grid
  ( Grid,
    empty,
    insert,
    member,
    getWithDefault,
    gridBounds,
    Point (..),
    pointX,
    pointY,
    Bounds,
    minX,
    maxX,
    minY,
    maxY,
  )
where

import qualified Data.Map as M (Map, empty, findWithDefault, insert, keys, member)

data Point = Point Int Int deriving (Eq, Ord)

instance Show Point where
  show (Point x y) = "(" ++ show x ++ "," ++ show y ++ ")"

pointX :: Point -> Int
pointX (Point x _) = x

pointY :: Point -> Int
pointY (Point _ y) = y

newtype Grid v = Grid (M.Map Point v) deriving (Eq)

empty :: Grid v
empty = Grid M.empty

insert :: Point -> v -> Grid v -> Grid v
insert k v (Grid m) = Grid (M.insert k v m)

member :: Point -> Grid a -> Bool
member k (Grid m) = M.member k m

getWithDefault :: a -> Point -> Grid a -> a
getWithDefault a k (Grid m) = M.findWithDefault a k m

-- Holds the min/max of each coordinate in a Grid
data Bounds = Bounds
  { minX :: Int,
    maxX :: Int,
    minY :: Int,
    maxY :: Int
  }
  deriving (Eq, Show)

-- Returns the Bounds of a Grid
gridBounds :: Grid v -> Bounds
gridBounds (Grid m) =
  let points = M.keys m
   in Bounds
        { minX = minimum (map pointX points),
          maxX = maximum (map pointX points),
          minY = minimum (map pointY points),
          maxY = maximum (map pointY points)
        }
