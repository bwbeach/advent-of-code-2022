module Main (main) where

import Data.Char
import Data.List.Split
import qualified Data.Map.Strict as M
import Data.Maybe
import Debug.Trace
import Linear.Metric
import Linear.V2 (V2 (..))
import Linear.V3 (V3 (..), cross)
import MyLib

main :: IO ()
main = do
  runWithFile "test.txt"

-- runWithFile "input.txt"

runWithFile :: String -> IO ()
runWithFile fileName = do
  putStrLn fileName
  text <- readFile fileName
  let (grid, instructions) = parseInput text
  let result = day22a grid instructions
  putStrLn . unlines . gridToStrings $ markGrid grid result
  print . day22Code . last $ result

-- putStrLn . cubeToString . makeCube $ grid

-- | Mark positions and directions on the grid
markGrid :: Foldable t => Grid -> t (V2 Int, Dir) -> Grid
markGrid =
  foldl markOne
  where
    markOne g (p, d) = gridInsert p (dirChar d) g

-- | As we're moving around, our state is position and direction.
type State = (Pos, Dir)

-- | Given the input grid, and a sequence of instructions, return a sequence of States.
--
-- Each state is a position and direction.  The returned list is all of the (pos, dir)
-- pairs from the starting location to the end.
day22a :: Grid -> [Instruction] -> [State]
day22a grid instructions =
  startState : scanl applyInstruction startState instructions
  where
    startState = (startingPoint grid, right)

    applyInstruction :: State -> Instruction -> State
    applyInstruction s@(pos0, dir0) instr =
      case instr of
        TurnLeft -> (pos0, turnLeft dir0)
        TurnRight -> (pos0, turnRight dir0)
        Move n -> move s n

    -- \| Move n positions form where we are in the direction we're facing, or until hitting a wall.
    move :: State -> Int -> State
    move s 0 = s
    move s@(pos, dir) n =
      case look nextPos of
        Nothing -> error "should have wrapped"
        Just '#' -> s
        Just _ -> move (nextPos, dir) (n - 1)
      where
        candidatePos = pos + dir
        needToWrap = isEmpty candidatePos
        nextPos = if needToWrap then wrap pos dir else candidatePos
        isEmpty p = isNothing $ look p
        look p = gridLookup p grid
        -- wrap around from a spot that is non-empty.  the problem description says
        -- to "look in the opposite direction" until finding the edge of the board
        wrap p d =
          if isEmpty p' then p else wrap p' d
          where
            p' = p + turnAround d

-- | The code that is the answer, derived from the ending position and direction.
day22Code :: State -> Int
day22Code (V2 x y, d) = 1000 * (y + 1) + 4 * (x + 1) + dirCode d

parseInput :: String -> (Grid, [Instruction])
parseInput text =
  case splitOn [""] . lines $ text of
    [gridText, instructionsText] -> (gridFromStrings (V2 0 0) gridText, parseInstructions (concat instructionsText))
    _ -> error ("bad input: " ++ text)

data Instruction = TurnLeft | TurnRight | Move Int deriving (Eq, Show)

parseInstructions :: String -> [Instruction]
parseInstructions [] = []
parseInstructions (c : cs)
  | isDigit c = parseMove (c : cs)
  | c == 'L' = TurnLeft : parseInstructions cs
  | c == 'R' = TurnRight : parseInstructions cs
  | c == '\n' = parseInstructions cs
  | otherwise = error ("bad instructions: " ++ (c : cs))

parseMove :: [Char] -> [Instruction]
parseMove s =
  Move (read ns) : parseInstructions rest
  where
    (ns, rest) = span isDigit s

-- | Find the initial position: the first '.' on the top row
startingPoint :: Grid -> Pos
startingPoint g =
  head . filter isDot . iterate (+ V2 1 0) $ gridTopLeft g
  where
    isDot p = gridLookup p g == Just '.'

-- | A location on the grid
type Pos = V2 Int

-- | A direction is represented as a unit vector
type Dir = V2 Int

up :: Dir
up = V2 0 (-1)

down :: Dir
down = V2 0 1

left :: Dir
left = V2 (-1) 0

right :: Dir
right = V2 1 0

turnLeft :: Dir -> Dir
turnLeft (V2 x y) = V2 y (negate x)

turnRight :: Dir -> Dir
turnRight (V2 x y) = V2 (negate y) x

turnAround :: Dir -> Dir
turnAround (V2 x y) = V2 (negate x) (negate y)

dirChar :: Dir -> Char
dirChar (V2 1 0) = '>'
dirChar (V2 (-1) 0) = '<'
dirChar (V2 0 (-1)) = '^'
dirChar (V2 0 1) = 'v'
dirChar v = error ("bad direction: " ++ show v)

dirCode :: Dir -> Int
dirCode (V2 1 0) = 0
dirCode (V2 (-1) 0) = 2
dirCode (V2 0 (-1)) = 3
dirCode (V2 0 1) = 1
dirCode v = error ("bad direction: " ++ show v)

traceIt :: Show a => [Char] -> a -> a
traceIt lbl x = trace (lbl ++ " " ++ show x) x

-- | A location in 3D
type Pos3 = V3 Int

point3X (V3 x _ _) = x

point3Y (V3 _ y _) = y

point3Z (V3 _ _ z) = z

-- | A direction in 3D -- a unit vector.
type Dir3 = V3 Int

-- | Orientation of the walker in 3D, defined by a forward unit vector, and an "up" unit vector.
type Orient3 = (Dir3, Dir3)

turnRight3 :: Orient3 -> Orient3
turnRight3 (f, u) = (f `cross` u, u)

turnLeft3 :: Orient3 -> Orient3
turnLeft3 (f, u) = (u `cross` f, u)

-- | Transform the grid into a 3D cube, represented as a map from 3D position to info.
--
-- The info for each position on each face is (c, up) where c is the character
-- from the original grid, and up is the vector pointing in the direction of up
-- on the original grid, rotated to match the orientation of the face.
--
-- Works by simulating walking around the grid with the "explore" function,
-- while also tracking a position/orientation in 3D.  When walking off the
-- edge of a face, the explorer pitches forward 90 degrees to walk on the
-- adjacent face.  The explorer gets full coverage by trying going right,
-- straight, and left from every position, with the "explore" function ignoring
-- duplicate positions found.
makeCube :: Grid -> M.Map Pos3 (Char, Pos, Dir3)
makeCube grid =
  -- The state of the explorer is a tuple containing:
  --  - current 2d position on original grid
  --  - current 2d direction
  --  - current 3d position
  --  - current 3d orientation
  --  - current 3d direction of "up" on the grid
  explore getKey getValue getSuccessors start
  where
    -- how big is each face?
    -- there are six faces, each of which is a square
    faceSize = intSqrt (gridPointCount grid `div` 6)
    -- starting point on the input grid
    start2d = startingPoint grid
    -- starting point on the output cube
    start3d = V3 0 0 0
    -- starting orientation on the cube: looking right, standing perpendicular to the face
    startOrient = (V3 1 0 0, V3 0 0 (-1))
    -- initial state of the explorer
    start = (start2d, right, start3d, startOrient, V3 0 (-1) 0)
    -- the key for the output map is the 3D position of the explorer
    getKey (_, _, p3, _, _) = p3
    -- the information stored in the output map is the char from the grid, and the direction of grid "up"
    getValue (p2, _, _, _, u3) = (fromJust $ gridLookup p2 grid, p2, u3)
    -- what are the adjacent places the explorer can get to by going straight one unit,
    -- turning left and going one unit, or turning right and going one unit?
    getSuccessors (p2, d2, p3, o3, gu3) =
      filter
        has2dValue
        [ moveOne (p2, d2, p3, o3, gu3),
          moveOne (p2, turnLeft d2, p3, turnLeft3 o3, gu3),
          moveOne (p2, turnRight d2, p3, turnRight3 o3, gu3)
        ]
    -- is the explorer on a defined point in the input grid?
    has2dValue (p2, _, _, _, _) = isJust $ gridLookup p2 grid
    -- move the explorer's state by one unit, possibly rotating onto
    -- an adjacent face.
    moveOne (p2, d2, p3, o3@(f3, u3), gu3) =
      if oneSameFace p2 (p2 + d2)
        then (p2 + d2, d2, p3 + f3, o3, u3)
        else (p2 + d2, d2, p3 + f3 - u3, pitchForward o3, rotate gu3 (f3 `cross` u3))
    -- Are the two 2d points on the same cube face?
    oneSameFace (V2 x1 y1) (V2 x2 y2) =
      (x1 `div` faceSize == x2 `div` faceSize)
        && (y1 `div` faceSize == y2 `div` faceSize)
    -- Rotate a 3d orientation by pitching forward 90 degrees
    pitchForward (f, u) = (negate u, f)
    -- Rotate "a" 90 degrees clockwise around "b"
    rotate :: V3 Int -> V3 Int -> V3 Int
    rotate a b = (a `cross` b) + fmap (* (a `dot` b)) b

intSqrt :: Int -> Int
intSqrt 16 = 4
intSqrt 2500 = 50
intSqrt n = error ("intSqrt not implemented for: " ++ show n)

cubeToString :: M.Map Pos3 (Char, Pos, Dir3) -> String
cubeToString m =
  concatMap planeToString [minZ .. maxZ]
  where
    ((minX, maxX), (minY, maxY), (minZ, maxZ)) = cubeBounds m
    planeToString z = concatMap (rowToString z) [minY .. maxY] ++ "\n"
    rowToString z y = map (cellToChar z y) [minX .. maxX] ++ "\n"
    cellToChar z y x = maybe ' ' valueToChar (M.lookup (V3 x y z) m)
    valueToChar (c, _, _) = c

type MinMax = (Int, Int)

cubeBounds :: M.Map Pos3 a -> (MinMax, MinMax, MinMax)
cubeBounds m =
  ( minMax . map point3X . M.keys $ m,
    minMax . map point3Y . M.keys $ m,
    minMax . map point3Z . M.keys $ m
  )

minMax :: [Int] -> MinMax
minMax ns = (minimum ns, maximum ns)