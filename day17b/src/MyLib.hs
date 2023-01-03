module MyLib (Cave, Rock, addRock, parseRocks, moveRockLeft, moveRockRight, Jet (..), parseJets) where

import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)

-- | The state of the computation.
-- Includes positions in the sequences of rocks and jets, and the contents of the cave.

-- | A Rock is a list of strings, starting at the top and going down.
--
-- The Rock includes its horizonal position as leading spaces in each string.
-- A '#' in the string means there's rock there, anything else means not.
type Rock = [String]

-- | A Cave is a list of strings, starting at the top and going down.
--
-- A '#' in the string means there's rock there, anything else means not.
type Cave = [String]

-- | Converts the contents of rocks.txt into a list of Rocks
parseRocks :: String -> [Rock]
parseRocks = splitOn [""] . lines

-- | Moves a rock to the left, if possible
moveRockLeft :: Rock -> Rock
moveRockLeft r =
  if all startsWithSpace r
    then map tail r
    else r
  where
    startsWithSpace s =
      case s of
        (' ' : _) -> True
        _ -> False

-- | Moves a rock to the right, if possible
moveRockRight :: Rock -> Rock
moveRockRight r =
  if maximum (map length r) < 7
    then map (" " ++) r
    else r

-- | A hot air jet
data Jet = JetLeft | JetRight deriving (Show)

-- | Converts input to a list of jets
parseJets :: String -> [Jet]
parseJets =
  mapMaybe convertChar
  where
    convertChar c = case c of
      '<' -> Just JetLeft
      '>' -> Just JetRight
      '\n' -> Nothing
      _ -> error ("bad jet " ++ [c])

-- | Adds a rock to a cave, if there are no collisions.
addRock :: Rock -> Int -> Cave -> Maybe Cave
addRock rock offset cave
  | offset < 0 = merge (replicate (negate offset) "" ++ rock) cave
  | 0 < offset = merge rock (replicate offset "" ++ cave)
  | otherwise = merge rock cave

merge :: [String] -> [String] -> Maybe [String]
merge = zipMaybeWithTail mergeRow

mergeRow :: String -> String -> Maybe String
mergeRow = zipMaybeWithTail mergeChar

mergeChar :: Char -> Char -> Maybe Char
mergeChar '#' '#' = Nothing
mergeChar '#' _ = Just '#'
mergeChar _ '#' = Just '#'
mergeChar _ _ = Just ' '

-- | Like `zipWith`, but ...
--     - the function returns a Maybe, and the result is Nothing if any are Nothing
--     - if one list is longer, the remainder is kept in the result
zipMaybeWithTail :: (a -> a -> Maybe a) -> [a] -> [a] -> Maybe [a]
zipMaybeWithTail _ as [] = Just as
zipMaybeWithTail _ [] bs = Just bs
zipMaybeWithTail f (a : as) (b : bs) = (:) <$> f a b <*> zipMaybeWithTail f as bs
