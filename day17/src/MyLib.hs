module MyLib (Rock, parseRocks, showRock) where

import Data.List.Split (splitOn)
import qualified Data.Set as S
import Linear.V2 (V2 (..))

-- | A rock is a set of points
--
-- The range of X values start and 0 and increases to the right
-- The range of Y values starts at 0 and increases down
type Rock = S.Set (V2 Int)

-- | Parses the text from the problem that specifies the rock shapes.
parseRocks :: String -> [Rock]
parseRocks = map parseRock . splitOn [""] . lines

parseRock :: [String] -> Rock
parseRock rows =
  S.fromList
    [ V2 x y
      | (y, row) <- zip [0 ..] rows,
        (x, c) <- zip [0 ..] row,
        c == '#'
    ]

-- | Returns a multi-line string displaying a rock
showRock :: Rock -> String
showRock r =
  unlines rows
  where
    maxX = maximum [x | (V2 x _) <- S.toList r]
    maxY = maximum [y | (V2 _ y) <- S.toList r]
    rows = [row y | y <- [0 .. maxY]]
    row y = [oneChar (V2 x y) | x <- [0 .. maxX]]
    oneChar p = if S.member p r then '#' else '.'
