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
    explore,
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

-- | Explores terrain, capturing information about the locations found.
--
-- Type "a" is the current state of the explorer, which may include more
-- information than will be stored in the resulting map.  The current
-- direction, for example, might not be stored in the map.
--
-- Unlike a normal graph traversal, this function supports the state of the
-- explorer being different than the nodes.  It just requires a function,
-- getValue to get the current node from the explorer's state.
explore ::
  Ord k =>
  (a -> k) -> -- function that extracts the map key from a state
  (a -> v) -> -- function that extracts the map value from a state
  (a -> [a]) -> -- successors of a state: zero or more
  a -> -- start state
  M.Map k v -- for all keys found, map from key to value
explore getKey getValue getSuccessors start =
  go M.empty [start]
  where
    go mapSoFar toVisit =
      case toVisit of
        [] -> mapSoFar
        (v : vs) -> let (m', vs') = maybeVisit mapSoFar v in go m' (vs' ++ vs)
    -- Visit "a" if we haven't been there before.
    maybeVisit m a =
      if M.member (getKey a) m
        then (m, [])
        else (M.insert (getKey a) (getValue a) m, getSuccessors a)