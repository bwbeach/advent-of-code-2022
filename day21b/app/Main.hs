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
  print . day20b $ input

day20a :: M.Map Name Expr -> Integer
day20a input = fromJust $ eval input "root"

day20b :: M.Map Name Expr -> Integer
day20b input =
  findUnknown modifiedInput "root" 1
  where
    modifiedInput =
      M.insert "root" (changeToEql . fromJust . M.lookup "root" $ input) $
        M.insert "humn" Unknown $
          input

-- | Monkey names are strings
type Name = String

-- | Each monkey has an expression that defines what its number is
data Expr
  = Const Integer
  | Add Name Name
  | Sub Name Name
  | Mul Name Name
  | Div Name Name
  | Eql Name Name
  | Unknown
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
--
-- Returns the number if known, or Nothing if one if the inputs is unknown.
eval :: M.Map Name Expr -> Name -> Maybe Integer
eval defs name =
  case fromJust . M.lookup name $ defs of
    Const n -> Just n
    Add a b -> Just (+) <*> go a <*> go b
    Sub a b -> Just (-) <*> go a <*> go b
    Mul a b -> Just (*) <*> go a <*> go b
    Div a b -> Just div <*> go a <*> go b
    Eql a b -> Just eql <*> go a <*> go b
    Unknown -> Nothing
  where
    go = eval defs

-- | Equality that produces an Integer
eql :: Integer -> Integer -> Integer
eql a b = if a == b then 1 else 0

-- | Change the operator in an expression to "="
changeToEql :: Expr -> Expr
changeToEql expr =
  case expr of
    Add a b -> Eql a b
    Sub a b -> Eql a b
    Mul a b -> Eql a b
    Div a b -> Eql a b
    _ -> error ("cannot change to eql: " ++ show expr)

-- | Find the unknown value in an expression that has a known result.
findUnknown :: M.Map Name Expr -> Name -> Integer -> Integer
findUnknown defs name result =
  case def of
    Const _ -> error ("findUnknown used on: " ++ show def)
    Add a b -> findBinary a b addInput addInput
    Sub a b -> findBinary a b subLeft subRight
    Mul a b -> findBinary a b mulInput mulInput
    Div a b -> findBinary a b divLeft divRight
    Eql a b -> findBinary a b eqlInput eqlInput
    Unknown -> result
  where
    def = fromJust . M.lookup name $ defs

    -- Determine the unknown value that feeds into one input of a binary operator
    -- "fLeft" is a function that determines what the left input must be, given the result and the right input
    -- "fRight" is a function that determines what the right input must be, given the result and the left input
    findBinary :: Name -> Name -> (Integer -> Integer -> Integer) -> (Integer -> Integer -> Integer) -> Integer
    findBinary a b fLeft fRight =
      case (eval defs a, eval defs b) of
        (Just a', Nothing) -> findUnknown defs b (fRight result a')
        (Nothing, Just b') -> findUnknown defs a (fLeft result b')
        (a', b') -> error ("findBinary " ++ show result ++ " on " ++ show def ++ " " ++ show a' ++ " " ++ show b')

    eqlInput 1 x = x
    eqlInput r _ = error ("eqlInput with result: " ++ show r)

    addInput r n = r - n

    subLeft r b = r + b

    subRight r a = a - r

    mulInput p n = p `div` n

    divLeft r b = r * b

    divRight r a = a `div` r
