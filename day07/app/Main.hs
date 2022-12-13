{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-incomplete-uni-patterns #-}

module Main where

import Data.Char (isDigit)
import qualified Data.Map.Strict as Map
import Debug.Trace

main :: IO ()
main = do
  runInput "test.txt"
  runInput "input.txt"

runInput :: String -> IO ()
runInput fileName = do
  input <- readFile fileName
  putStrLn fileName
  print . day07 $ input
  print . day07b $ input

day07 :: String -> Int
day07 text =
  let State dir _ = foldl processLine (State emptyDir []) (lines text)
   in sum . map entrySize . filter isSmallDir $ allDirs dir

day07b :: String -> Int
day07b text =
  let State dir _ = foldl processLine (State emptyDir []) (lines text)
      (Dir dirSize _) = dir
      totalSpace = 70000000 :: Int
      unusedSpace = totalSpace - dirSize
      neededSpace = 30000000 - unusedSpace
   in minimum . filter (>= neededSpace) . map entrySize $ allDirs dir

isSmallDir :: DirEntry -> Bool
isSmallDir (File _) = False
isSmallDir (Dir s _) = s <= 100000

-- Directory names are represented as a list of segment names
-- "/" is []
-- "/a/b" is ["a", "b"]
type Path = [String]

-- Directory entries are either directories or plain files
data DirEntry = File Int | Dir Int (Map.Map String DirEntry)

instance Show DirEntry where
  show d = "\n" ++ unlines (formatDirEntry 0 ("/", d)) ++ "\n"

-- An empty directory
emptyDir :: DirEntry
emptyDir = Dir 0 Map.empty

-- The state of reading directories: the known dir tree, and the current directory
data State = State DirEntry Path deriving (Show)

-- Creates the line naming a directory (to be followed by lines for its contents)
formatDirLine :: Int -> Int -> String -> String
formatDirLine depth s name = prefix depth ++ name ++ " (dir, size=" ++ show s ++ ")"

-- Converts a directory entry to display format
formatDirEntry :: Int -> (String, DirEntry) -> [String]
formatDirEntry depth (name, File s) = [prefix depth ++ name ++ " (file, size=" ++ show s ++ ")"]
formatDirEntry depth (name, Dir s m) =
  formatDirLine depth s name : concatMap (formatDirEntry (depth + 1)) (Map.assocs m)

-- Makes the prefix to put on one formatted line
prefix :: Int -> String
prefix depth = concat (replicate depth "  ") ++ "- "

-- Process one input line and update a State
processLine :: State -> String -> State
processLine stateBefore line = processWords stateBefore (words line)

processWords :: State -> [String] -> State
processWords (State d _) ["$", "cd", "/"] = State d []
processWords (State d p) ["$", "cd", ".."] = State d (butLast p)
processWords (State d p) ["$", "cd", dirName] = State d (p ++ [dirName])
processWords state ["$", "ls"] = state
processWords (State d p) ["dir", dirName] = State (addEntry p dirName emptyDir d) p
processWords (State d p) ws
  | all isDigit (head ws) =
      let [sizeStr, fileName] = ws in State (addEntry p fileName (File (read sizeStr)) d) p
  | otherwise = trace ("BAD " ++ unwords ws) (State d p)

addEntry :: Path -> String -> DirEntry -> DirEntry -> DirEntry
addEntry [] name entry (Dir s m) = Dir (s + entrySize entry) (Map.insert name entry m)
addEntry (p : ps) name entry (Dir s0 m) =
  let Just subDir0 = Map.lookup p m
      subDir1 = addEntry ps name entry subDir0
   in Dir (s0 + entrySize entry) (Map.insert p subDir1 m)

entrySize :: DirEntry -> Int
entrySize (File s) = s
entrySize (Dir s _) = s

butLast :: [a] -> [a]
butLast [_] = []
butLast (x : xs) = x : butLast xs

-- Returns all of the directories that are descendants of the given directory, including itself.
allDirs :: DirEntry -> [DirEntry]
allDirs (File _) = []
allDirs (Dir s m) =
  Dir s m : concatMap allDirs (Map.elems m)
