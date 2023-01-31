module Main (main) where

import Data.Char
import Data.List.Split
import Data.Maybe
import Debug.Trace
import Linear.V2 (V2 (..))
import Linear.V3 (V3 (..), cross)
import MyLib

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
  -- let (resultGrid, _, _) = result
  -- putStrLn "\n"
  -- putStrLn . unlines . gridToStrings $ resultGrid
  print . day22aCode $ result

day22a :: Grid -> [Instruction] -> State
day22a grid =
  foldl applyInstruction startState
  where
    startState = (grid, startingPoint grid, right)

day22aCode :: State -> Int
day22aCode (_, V2 x y, d) = 1000 * (y + 1) + 4 * (x + 1) + dirCode d

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
startingPoint :: Grid -> V2 Int
startingPoint g =
  head . filter isDot . iterate (+ V2 1 0) $ gridTopLeft g
  where
    isDot p = gridLookup p g == Just '.'

-- | As we're moving around, our state is the grid, plus our position and direction.
type State = (Grid, V2 Int, Dir)

applyInstruction :: State -> Instruction -> State
applyInstruction s@(grid, pos0, dir0) instr =
  case instr of
    TurnLeft -> (grid, pos0, turnLeft dir0)
    TurnRight -> (grid, pos0, turnRight dir0)
    Move n -> move s n

-- | Move n positions form where we are in the direction we're facing, or until hitting a wall.
move :: State -> Int -> State
move s 0 = s
move s@(grid, pos, dir) n =
  case look nextPos of
    Nothing -> error "should have wrapped"
    Just '#' -> s
    Just _ -> move (markedGrid, nextPos, dir) (n - 1)
  where
    candidatePos = pos + dir
    needToWrap = isEmpty candidatePos
    nextPos = if needToWrap then wrap pos dir else candidatePos
    markedGrid = gridInsert pos (dirChar dir) grid
    isEmpty p = isNothing $ look p
    look p = gridLookup p grid
    -- wrap around from a spot that is non-empty.  the problem description says
    -- to "look in the opposite direction" until finding the edge of the board
    wrap p d =
      if isEmpty p' then p else wrap p' d
      where
        p' = p + turnAround d

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

-- | A direction in 3D -- a unit vector.
type Dir3 = V3 Int

-- | Orientation of the walker in 3D, defined by a forward unit vector, and an "up" unit vector.
type Orient3 = (Dir3, Dir3)

turnRight3 :: Orient3 -> Orient3
turnRight3 (f, u) = (f `cross` u, u)

turnLeft3 :: Orient3 -> Orient3
turnLeft3 (f, u) = (u `cross` f, u)