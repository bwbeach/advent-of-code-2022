module MyLib
  ( Point,
    pointX,
    pointY,
    Rock,
    parseRocks,
    showRock,
    rockPoints,
    Cave,
    addRock,
    caveHeight,
    initialCave,
    showCave,
    Dir (..),
    parseJets,
    applyJet,
    applyGravity,
  )
where

import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)
import qualified Data.Set as S
import Linear.V2 (V2 (..))

-- | A point
-- positive x is to the right, positive y is down
type Point = V2 Int

pointY :: Point -> Int
pointY (V2 _ y) = y

pointX :: Point -> Int
pointX (V2 x _) = x

-- | A rock is a set of points
--
-- The range of X values start and 0 and increases to the right
-- The range of Y values starts at 0 and increases down
type Rock = S.Set Point

-- | Parses the text from the problem that specifies the rock shapes.
parseRocks :: String -> [Rock]
parseRocks = map parseRock . splitOn [""] . lines

parseRock :: [String] -> Rock
parseRock rows =
  S.fromList
    [ V2 x y
      | (y, row) <- zip [maxY, maxY - 1 ..] rows,
        (x, c) <- zip [0 ..] row,
        c == '#'
    ]
  where
    maxY = length rows - 1

-- | Returns the set of points a rock occupies if its position is `pos`
--
-- The position of a rock is its (0,0) point.
rockPoints :: Rock -> Point -> [Point]
rockPoints r pos = map (+ pos) (S.elems r)

-- | Returns the maximum X value in a rock
rockMaxX :: Rock -> Int
rockMaxX = maximum . map pointX . S.elems

-- | Returns the maximum X value in a rock
rockMaxY :: Rock -> Int
rockMaxY = maximum . map pointY . S.elems

-- | Returns a multi-line string displaying a rock
showRock :: Rock -> String
showRock r =
  unlines rows
  where
    maxX = maximum [x | (V2 x _) <- S.toList r]
    maxY = maximum [y | (V2 _ y) <- S.toList r]
    rows = [row y | y <- [maxY, maxY - 1 .. 0]]
    row y = [oneChar (V2 x y) | x <- [0 .. maxX]]
    oneChar p = if S.member p r then '#' else '.'

-- | A cave is represented as a set of points that have rock in them.
newtype Cave = Cave (S.Set Point)

-- | The starting cave has just a floor at height 0.
initialCave :: Cave
initialCave = Cave (S.fromList [V2 x 0 | x <- [0 .. 6]])

-- | Returns the Y value of the highest rock in the cave.
caveHeight :: Cave -> Int
caveHeight (Cave c) = maximum . map pointY . S.elems $ c

-- | Is the given rock, at the given position, colliding with the cave?
collision :: Cave -> Rock -> Point -> Bool
collision (Cave c) r p = any (`S.member` c) (rockPoints r p)

-- | Adds the given rock to the cave
addRock :: Cave -> Rock -> Point -> Cave
addRock (Cave c) r p = Cave (c `S.union` S.fromList (rockPoints r p))

-- | Make a human-readable cave.
showCave :: Cave -> String
showCave (Cave c) =
  unlines rows
  where
    rows = [""] ++ [row y | y <- [maxY, maxY - 1 .. 1]] ++ [bottomRow]
    row y = "|" ++ [caveChar (V2 x y) | x <- [0 .. maxX]] ++ "|"
    caveChar p = if S.member p c then '#' else '.'
    maxX = maximum . map pointX . S.elems $ c
    maxY = maximum . map pointY . S.elems $ c
    bottomRow = "+" ++ replicate 7 '-' ++ "+"

-- | Directions that jets can push rocks
data Dir = LeftJet | RightJet deriving (Show)

-- | Parses the input into a list of directions
parseJets :: String -> [Dir]
parseJets = mapMaybe parseJetChar

parseJetChar :: Char -> Maybe Dir
parseJetChar c =
  case c of
    '<' -> Just LeftJet
    '>' -> Just RightJet
    '\n' -> Nothing
    _ -> error ("bad jet char: " ++ [c])

-- | Apply a jet from the side to the position of a rock
applyJet :: Cave -> Rock -> Point -> Dir -> Point
applyJet c r p0 j =
  case j of
    LeftJet ->
      let p1 = p0 - V2 1 0
       in if pointX p0 == 0 || collision c r p1 then p0 else p1
    RightJet ->
      let p1 = p0 + V2 1 0
       in if pointX p0 + rockMaxX r == 6 || collision c r p1 then p0 else p1

-- | Moves the rock location down one, if it doesn't collide with the cave.
applyGravity :: Cave -> Rock -> Point -> Maybe Point
applyGravity c r p =
  if collision c r p'
    then Nothing
    else Just p'
  where
    p' = p - V2 0 1