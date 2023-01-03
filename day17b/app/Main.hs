{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main where

import Control.Monad.State
import Data.Maybe
import Debug.Trace
import MyLib

main :: IO ()
main = do
  runWithFile "test.txt"

runWithFile :: String -> IO ()
runWithFile fileName = do
  putStrLn fileName
  rockText <- readFile "rocks.txt"
  let rocks = parseRocks rockText
  inputText <- readFile fileName
  let jets = parseJets inputText
  print rocks
  print jets
  let rjc = execState (dropNRocks 2022) (initialRJC rocks jets)
  putStrLn (unlines (rjcCave rjc))
  print (length (rjcCave rjc) - 1)

initialRJC :: [Rock] -> [Jet] -> RJC
initialRJC rocks jets =
  RJC
    { rjcRocks = cycle rocks,
      rjcJets = cycle jets,
      rjcCave = ["#######"]
    }

data RJC = RJC
  { rjcRocks :: [Rock],
    rjcJets :: [Jet],
    rjcCave :: Cave
  }

dropNRocks :: Int -> State RJC ()
dropNRocks n =
  if n == 0
    then return ()
    else do
      dropRock
      dropNRocks (n - 1)

dropRock :: State RJC ()
dropRock = do
  r <- nextRock
  let r' = moveRockRight . moveRockRight $ r
  let y = 3 + length r'
  dropRockAt r' y

dropRockAt :: Rock -> Int -> State RJC ()
dropRockAt r y = do
  rjc <- get
  let c = rjcCave rjc
  jet <- nextJet
  let r' = applyJet jet r y c
  case applyGravity r' y c of
    Just y' -> dropRockAt (traceRock "DRA-r" r') y'
    Nothing -> do
      rjc' <- get
      let c' = fromJust $ addRock (traceRock "DRA-R" r') y c
      put rjc' {rjcCave = c'}
      return ()

traceRock :: String -> [String] -> [String]
traceRock lbl a = trace (lbl ++ "\n" ++ unlines a) a

traceIt :: Show a => String -> a -> a
traceIt lbl a = trace (lbl ++ " " ++ show a) a

applyGravity :: Rock -> Int -> Cave -> Maybe Int
applyGravity r y c =
  case addRock r (y - 1) c of
    Just _ -> Just (y - 1)
    _ -> Nothing

applyJet :: Jet -> Rock -> Int -> Cave -> Rock
applyJet jet r y c =
  case addRock r' y c of
    Just _ -> r'
    _ -> r
  where
    r' = case jet of
      JetLeft -> moveRockLeft r
      JetRight -> moveRockRight r

nextJet :: State RJC Jet
nextJet = do
  s <- get
  let (j : js) = rjcJets s
  put (s {rjcJets = js})
  return j

nextRock :: State RJC Rock
nextRock = do
  s <- get
  let (r : rs) = rjcRocks s
  put (s {rjcRocks = rs})
  return r

-- dropOneRock :: ([Rock], [Jet], Cave) -> ([Rock], [Jet], Cave)
-- dropOneRock (rocks, jets, cave) =
--   (rocks', jets', cave')
--   where
--     (rock, rocks') = takeFirst rocks
--     initialOffset = length r + 3
--     (jets', cave') = dropFrom rock initialOffset cave
--     dropFrom r0 offset c = 0

-- applyJet :: [Jet] -> Rock -> Int -> Cave -> ([Jet], Rock)
-- applyJet jets r offset c =
--   let (j, jets') = takeFirst jets
--       r' = case j of
--         JetLeft -> moveRockLeft r
--         JetRight -> moveRockRight r
--    in case addRock r' offset c of
--         Nothing -> (jets', r)
--         Just _ -> (jets', r')

-- takeFirst :: [a] -> (a, [a])
-- takeFirst as = (head as, tail as)
