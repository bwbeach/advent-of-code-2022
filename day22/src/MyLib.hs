module MyLib
  ( Grid,
    gridFromList,
    gridFromStrings,
    gridToStrings,
    gridLookup,
    gridInsert,
    gridTopLeft,
    pointX,
    pointY,
  )
where

import qualified Data.Map.Strict as M
import Data.Maybe
import Linear.V2 (V2 (..))

-- | A possibly sparse grid of characters.
--
-- Cells are named with (V2 Int) vectors containing x and y coordinates.
-- Right is positive x, down is positive y.
--
-- When printing and showing, a cell with no value is represented as a space.
data Grid = Grid
  { gridMinX :: Int,
    gridMaxX :: Int,
    gridMinY :: Int,
    gridMaxY :: Int,
    gridCells :: M.Map (V2 Int) Char
  }

instance Show Grid where
  showsPrec d g =
    showParen (d > prec) $
      showString "gridFromStrings "
        . showsPrec (prec + 1) (V2 (gridMinX g) (gridMinY g))
        . showString " "
        . showsPrec (prec + 1) (gridToStrings g)
    where
      prec = 10

gridTopLeft :: Grid -> V2 Int
gridTopLeft g = V2 (gridMinX g) (gridMinY g)

gridToStrings :: Grid -> [String]
gridToStrings g =
  map gridLine [gridMinY g .. gridMaxY g]
  where
    gridLine y = map (gridCell y) [gridMinX g .. gridMaxX g]
    gridCell :: Int -> Int -> Char
    gridCell y x = fromMaybe ' ' . M.lookup (V2 x y) . gridCells $ g

-- | Creates a grid from a list of (pos, value) pairs.
gridFromList :: [(V2 Int, Char)] -> Grid
gridFromList items =
  Grid
    { gridMaxX = maximum . map pointX $ points,
      gridMaxY = maximum . map pointY $ points,
      gridMinX = minimum . map pointX $ points,
      gridMinY = minimum . map pointY $ points,
      gridCells = M.fromList items
    }
  where
    points = map fst items

-- | Creates a grid of characters by parsing a grid of characters in strings.
gridFromStrings :: V2 Int -> [String] -> Grid
gridFromStrings (V2 minX minY) strs =
  Grid
    { gridMaxX = minX + maximum (map length strs) - 1,
      gridMaxY = minY + length strs - 1,
      gridMinX = minX,
      gridMinY = minY,
      gridCells = M.fromList items
    }
  where
    items = concat $ zipWith lineItems [minY ..] strs
    lineItems y str = catMaybes $ zipWith (oneItem y) [minX ..] str
    oneItem y x c
      | c == ' ' = Nothing
      | otherwise = Just (V2 x y, c)

pointX :: V2 Int -> Int
pointX (V2 x _) = x

pointY :: V2 Int -> Int
pointY (V2 _ y) = y

-- | Returns the character at the given position
gridLookup :: V2 Int -> Grid -> Maybe Char
gridLookup p g = M.lookup p (gridCells g)

-- | Sets the character at the given position
gridInsert :: V2 Int -> Char -> Grid -> Grid
gridInsert p@(V2 x y) c g =
  g
    { gridMaxX = max x (gridMaxX g),
      gridMaxY = max y (gridMaxY g),
      gridMinX = min x (gridMinX g),
      gridMinY = min y (gridMinY g),
      gridCells = M.insert p c (gridCells g)
    }