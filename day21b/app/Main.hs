module Main where

import Data.List (delete)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)

main :: IO ()
main = do
  runWithFile "test.txt"
  runWithFile "input.txt"

runWithFile :: String -> IO ()
runWithFile fileName = do
  putStrLn fileName
  text <- readFile fileName
  let input = parseInput text
  print . day20a $ input

day20a :: M.Map Name Expr -> Integer
day20a input = eval input "root"

-- | Monkey names are strings
type Name = String

-- | Each monkey has an expression that defines what its number is
data Expr
  = Const Integer
  | Add Name Name
  | Sub Name Name
  | Mul Name Name
  | Div Name Name
  deriving (Show, Eq)

-- | Parse a problem spec
parseInput :: String -> M.Map Name Expr
parseInput = M.fromList . map parseInputLine . lines

-- | Parse one input line, returning the monkey name, and the expression for it.
parseInputLine :: String -> (Name, Expr)
parseInputLine text =
  case words . delete ':' $ text of
    name : rest -> (name, parseExpr rest)
    _ -> error ("bad line: " ++ text)

parseExpr :: [String] -> Expr
parseExpr items =
  case items of
    [numStr] -> Const (read numStr)
    [a, "+", b] -> Add a b
    [a, "-", b] -> Sub a b
    [a, "*", b] -> Mul a b
    [a, "/", b] -> Div a b
    other -> error ("bad expr " ++ show other)

-- | Evaluate the number yelled by one monkey
eval :: M.Map Name Expr -> Name -> Integer
eval defs name =
  case fromJust . M.lookup name $ defs of
    Const n -> n
    Add a b -> go a + go b
    Sub a b -> go a - go b
    Mul a b -> go a * go b
    Div a b -> go a `div` go b
  where
    go = eval defs
