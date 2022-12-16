module Main where

import Debug.Trace (trace)

main :: IO ()
main = do
  runInput "test.txt"
  runInput "input.txt"

runInput :: String -> IO ()
runInput fileName = do
  input <- readFile fileName
  putStrLn fileName
  print . day10 $ input

day10 :: String -> Int
day10 text =
  let program = compile . parseInput $ text
      xValues = 1 : knitl executeOne 1 program
   in sum (map (\n -> trace ("AAA " ++ show (xValues !! n) ++ " " ++ show n) (xValues !! n) * n) [20, 60, 100, 140, 180, 220])

data Instruction
  = Noop
  | AddX Int

parseInput :: String -> [Instruction]
parseInput text =
  map parseLine (lines text)

parseLine :: String -> Instruction
parseLine text =
  let ws = words text
   in if ws == ["noop"]
        then Noop
        else
          if head ws == "addx"
            then AddX (read (ws !! 1))
            else error ("Bad instruction " ++ text)

-- Translates the program as given into one instruction per clock cycle
compile :: [Instruction] -> [Instruction]
compile [] = []
compile (Noop : is) = Noop : compile is
compile (AddX n : is) = Noop : AddX n : compile is

-- Executes one instruction
executeOne :: Int -> Instruction -> (Int, Int)
executeOne prevX Noop = (prevX, prevX)
executeOne prevX (AddX n) =
  let newX = prevX + n
   in (prevX, newX)

-- Uses function f to map every element of a list, and weave a value from left to right.
--
-- Output of the function is a mapped value to include in the resulting list, and
-- a value to pass forward for the next element.
--
--          b0    b1    b2
--          |     |     |
--          v     v     v
--    a --> f --> f --> f
--          |     |     |
--          v     v     v
--          c0    c1    c2
--
knitl :: (a -> b -> (c, a)) -> a -> [b] -> [c]
knitl _ _ [] = []
knitl f a0 (b : bs) =
  let (c, a1) = f a0 b
   in (c : knitl f a1 bs)
