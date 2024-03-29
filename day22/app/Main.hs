module Main (main) where

import Data.Char (isDigit)
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust, isJust, isNothing)
import Linear.Metric (Metric (dot))
import Linear.V2 (V2 (..))
import Linear.V3 (V3 (..), cross)
import MyLib
  ( Grid,
    explore,
    gridFromStrings,
    gridInsert,
    gridLookup,
    gridPointCount,
    gridToStrings,
    gridTopLeft,
  )

main :: IO ()
main = do
  runWithFile "test.txt"
  runWithFile "input.txt"

runWithFile :: String -> IO ()
runWithFile fileName = do
  putStrLn fileName
  text <- readFile fileName
  let (grid, instructions) = parseInput text
  let result = day22a grid instructions
  -- putStrLn . unlines . gridToStrings $ markGrid grid result
  print . day22Code . last $ result
  let resultB = day22b grid instructions
  -- putStrLn . unlines . gridToStrings $ markGrid grid resultB
  print . day22Code . last $ resultB

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
  scanl applyInstruction startState (compileInstructions instructions)
  where
    startState = (startingPoint grid, right)

    applyInstruction s@(pos0, dir0) instr =
      case instr of
        TurnLeft -> (pos0, turnLeft dir0)
        TurnRight -> (pos0, turnRight dir0)
        Move 1 -> moveOne s
        _ -> error ("bad single instruction: " ++ show instr)

    -- \| Move one position form where we are in the direction we're facing, if there is no wall in the way.
    moveOne :: State -> State
    moveOne (pos, dir) =
      case look nextPos of
        Nothing -> error "should have wrapped"
        Just '#' -> (pos, dir)
        Just _ -> (nextPos, dir)
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

-- | State of walking around on the cube
type State3 = (Pos3, Orient3)

-- | Given the input grid, and a sequence of instructions, return a sequence of States.
--
-- Each state is a position and direction.  The returned list is all of the (pos, dir)
-- pairs from the starting location to the end.
--
-- This is like day22a, but the movement is done on the cube, not on the flat grid.
--
-- The state we track is the position and orientation on the grid: (Pos3, Orient3)
day22b :: Grid -> [Instruction] -> [State]
day22b grid instructions =
  map flattenState $ scanl applyInstruction start (compileInstructions instructions)
  where
    -- We need a cube to walk around on
    cube = makeCube grid

    -- We start at the origin, facing right, standing up
    start = (V3 0 0 0, (V3 1 0 0, V3 0 0 (-1)))

    -- Apply one instruction by moving on the cube
    applyInstruction :: State3 -> Instruction -> State3
    applyInstruction s@(pos0, orient0) instr =
      case instr of
        TurnLeft -> (pos0, turnLeft3 orient0)
        TurnRight -> (pos0, turnRight3 orient0)
        Move 1 -> moveOneMaybe s
        _ -> error ("bad single instruction: " ++ show instr)

    -- Move forward one unit, if that doesn't land on a '#'
    moveOneMaybe s =
      if isRock s'
        then s
        else s'
      where
        s' = moveOne s

    -- Check whether the cube at the given location is rock
    isRock (p, _) =
      c == '#'
      where
        (c, _, _) = fromJust (M.lookup p cube)

    -- Move forward one unit, possibly wrapping to another face
    moveOne :: State3 -> State3
    moveOne (pos0, (f0, u0)) =
      if M.member pos1 cube
        then (pos1, (f0, u0))
        else (pos2, (f2, u2))
      where
        -- position after moving forward one
        pos1 = pos0 + f0
        -- orientation after tipping forward 90 degrees
        (f2, u2) = (negate u0, f0)
        -- position after moving forward one, tipping forward, then moving forward again
        pos2 = pos1 + f2

    -- Turn a 3D state into a 2D state
    flattenState :: State3 -> State
    flattenState (pos, orient) =
      (p, d)
      where
        (_, p, gridUp) = fromJust $ M.lookup pos cube
        d = orientToDir orient gridUp

-- | Convert an orientation on the cube to a direction on the original grid
orientToDir :: Orient3 -> Dir3 -> Dir
orientToDir (f, u) gridUp
  | f == gridUp = up
  | f == negate gridUp = down
  | f `cross` u == gridUp = left
  | u `cross` f == gridUp = right
  | otherwise = error ("orientToDir failed: " ++ show f ++ " " ++ show u ++ " " ++ show gridUp)

-- | Translates an instruction into a sequence of single steps.
--
-- In the result, every move instructions is (Move 1)
compileInstructions :: [Instruction] -> [Instruction]
compileInstructions =
  concatMap compileOne
  where
    compileOne (Move n) = replicate n (Move 1)
    compileOne i = [i]

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
-- The info for each position on each face is (c, pos, up) where c is the character
-- from the original grid, pos is the position on the original grid, and up is the
-- vector pointing in the direction of up on the original grid, rotated to match the
-- orientation of the face.
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
        then (p2 + d2, d2, p3 + f3, o3, gu3)
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