module Main (main) where

import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromMaybe, listToMaybe, mapMaybe)
import qualified Data.Set as S
import Linear.V2 (V2 (..))
import MyLib ()

main :: IO ()
main = do
  runWithFile "tinyTest.txt"
  runWithFile "test.txt"
  runWithFile "input.txt"

runWithFile :: FilePath -> IO ()
runWithFile fileName = do
  input <- readFile fileName
  let grid = parseGrid input
  let (grid', _) = oneRound (grid, initialSearchOrder)
  putStrLn . formatGrid $ grid
  putStrLn . formatGrid $ grid'
  let (grid'', _) = applyNTimes 10 oneRound (grid, initialSearchOrder)
  print . emptyTiles $ grid''

applyNTimes :: Int -> (a -> a) -> a -> a
applyNTimes n f x = iterate f x !! n

emptyTiles :: S.Set (V2 Int) -> Int
emptyTiles grid = gridSize grid - S.size grid

-- | The position of an elf.
type Pos = V2 Int

-- | Does one round of moves
oneRound :: (S.Set Pos, SearchOrder) -> (S.Set Pos, SearchOrder)
oneRound (elves, searchOrder) =
  (S.fromList actuals, rotate searchOrder)
  where
    actuals = concatMap evaluateProposal . M.toList $ proposalMap

    -- decide what to do about proposals to move to a given place
    -- Returns a list of elf locations
    evaluateProposal (dest, sources) =
      if S.size sources == 1
        then -- only one elf is proposing to move to this place, so do it
          [dest]
        else -- more than one elf wants to move here; they all stay in their original places
          S.toList sources

    -- map from location to elves that propose going there
    -- includes "proposals" for elves that are staying in place
    proposalMap = buildMap . map proposalPair . S.toList $ elves

    -- make a pair (dest, src) for the proposed move for an elf
    proposalPair p = (proposal p, p)

    -- where does one elf want to go, given their starting position?
    proposal p =
      if noNeighbors p
        then p
        else fromMaybe p . listToMaybe . mapMaybe (maybeGo p) $ searchOrder

    -- true iff an elf has no neighbors
    noNeighbors p =
      not . any (isNeighbor p) $ allDirections

    -- see if we want to propose going in the given direction
    maybeGo p ds =
      if allEmpty p ds
        then Just (p + head ds)
        else Nothing

    -- are all of the given neighbors empty?
    allEmpty p = not . any (isNeighbor p)

    -- is the given neighbor present?
    isNeighbor p d = S.member (p + d) elves

-- | Build a map where the values are sets of things appearing a seconds in pairs.
buildMap :: (Ord a, Ord b) => [(a, b)] -> M.Map a (S.Set b)
buildMap =
  foldl addPair M.empty
  where
    addPair m (a, b) = updateMap S.empty a (S.insert b) m

-- | Replaces a map entry by applying a function to the previous value, or a default value if there is no current value.
updateMap ::
  Ord k =>
  v -> -- default value
  k -> -- key
  (v -> v) -> -- value updater
  M.Map k v -> -- original map
  M.Map k v -- updated map
updateMap d k f m =
  M.insert k (f v) m
  where
    v = fromMaybe d . M.lookup k $ m

-- | The order to search directions
type SearchOrder = [[V2 Int]]

-- | The initial choices of directions to move.
--
-- The first is the offset to the target, and the other two are the
-- other two cell to check for emptyness.
initialSearchOrder :: SearchOrder
initialSearchOrder =
  [ [n, nw, ne],
    [s, se, sw],
    [e, ne, se],
    [w, sw, nw]
  ]

-- | All possible
n :: V2 Int
n = V2 0 (-1)

nw :: V2 Int
nw = V2 1 (-1)

w :: V2 Int
w = V2 1 0

sw :: V2 Int
sw = V2 1 1

s :: V2 Int
s = V2 0 1

se :: V2 Int
se = V2 (-1) 1

e :: V2 Int
e = V2 (-1) 0

ne :: V2 Int
ne = V2 (-1) (-1)

-- | All eightn neighbors of a position
allDirections :: [Pos]
allDirections = [n, ne, e, se, s, sw, w, nw]

-- | Rotates a list by moving the first element to the end.
rotate :: [a] -> [a]
rotate (a : as) = as ++ [a]
rotate [] = error "cannot rotate an empty list"

parseGrid :: String -> S.Set Pos
parseGrid text =
  S.fromList . concat $ zipWith parseLine [1 ..] (lines text)
  where
    parseLine y str = catMaybes $ zipWith (parseCell y) [1 ..] str
    parseCell y x c = if c == '#' then Just (V2 x y) else Nothing

formatGrid :: S.Set Pos -> String
formatGrid grid =
  unlines . map formatLine $ [minY .. maxY]
  where
    (minX, maxX) = minMax . map pointX $ S.toList grid
    (minY, maxY) = minMax . map pointY $ S.toList grid
    formatLine y = map (formatCell y) [minX .. maxX]
    formatCell y x = if S.member (V2 x y) grid then '#' else '.'

gridSize :: S.Set (V2 Int) -> Int
gridSize grid =
  (maxX - minX + 1) * (maxY - minY + 1)
  where
    (minX, maxX) = minMax . map pointX $ S.toList grid
    (minY, maxY) = minMax . map pointY $ S.toList grid

minMax :: [Int] -> (Int, Int)
minMax ns = (minimum ns, maximum ns)

pointX :: V2 a -> a
pointX (V2 x _) = x

pointY :: V2 a -> a
pointY (V2 _ y) = y
