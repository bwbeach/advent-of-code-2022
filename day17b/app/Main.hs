{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main where

import Control.Monad.State
import qualified Data.Map.Strict as M
import Data.Maybe
import Debug.Trace
import MyLib

main :: IO ()
main = do
  runWithFile "test.txt"
  runWithFile "input.txt"

runWithFile :: String -> IO ()
runWithFile fileName = do
  putStrLn fileName
  rockText <- readFile "rocks.txt"
  let rocks = parseRocks rockText
  inputText <- readFile fileName
  let jets = parseJets inputText
  let rjc = execState (dropNRocks 2022) (initialRJC rocks jets)
  -- putStrLn (unlines (rjcCave rjc))
  print (rjcCheckpoints rjc)
  print (rjcRepeatHeight rjc + length (rjcCave rjc) - 1)

initialRJC :: [Rock] -> [Jet] -> RJC
initialRJC rocks jets =
  RJC
    { rjcRocks = cycle (zip [0 ..] rocks),
      rjcJets = cycle (zip [0 ..] jets),
      rjcCave = ["#######"],
      rjcPrevJetNumber = negate 1, -- TODO - use a Maybe
      rjcCheckpoints = M.empty,
      rjcRepeatHeight = 0
    }

data RJC = RJC
  { rjcRocks :: [(Int, Rock)],
    rjcJets :: [(Int, Jet)],
    rjcCave :: Cave,
    -- the number of the first jet on the previous rock
    rjcPrevJetNumber :: Int,
    -- map from (rockNumber, jetNumber, topNOfCave) to (numberRemaining, cave height)
    rjcCheckpoints :: M.Map (Int, Int, Cave) (Int, Int),
    -- height from repeats, not included in actual cave because we didn't actually do them
    rjcRepeatHeight :: Int
  }

rjcPeekJetNumber :: RJC -> Int
rjcPeekJetNumber = fst . head . rjcJets

rjcPeekRockNumber :: RJC -> Int
rjcPeekRockNumber = fst . head . rjcRocks

dropNRocks :: Int -> State RJC ()
dropNRocks n = do
  n' <- checkForRepeat n
  if n' == 0
    then return ()
    else do
      dropRock
      dropNRocks (n' - 1)

checkForRepeat :: Int -> State RJC Int
checkForRepeat n = do
  rjc <- get
  let jn = rjcPeekJetNumber rjc
  if jn < rjcPrevJetNumber rjc
    then do
      let c = rjcCave rjc
      let rn = rjcPeekRockNumber rjc
      let top = take 40 c
      let h = length c
      let k = (rn, jn, top)
      let cp = rjcCheckpoints rjc
      case M.lookup k cp of
        Nothing -> do
          put
            ( rjc
                { rjcCheckpoints = M.insert (rn, jn, top) (n, h) (rjcCheckpoints rjc),
                  rjcPrevJetNumber = jn
                }
            )
          return n
        Just (prevN, prevHeight) -> do
          let deltaH = traceIt "deltaH" $ h - prevHeight
          let deltaN = traceIt "deltaN" $ prevN - n
          let nRepeats = traceIt "nRepeats" $ n `div` deltaN
          put
            ( rjc
                { rjcCheckpoints = M.empty,
                  rjcPrevJetNumber = jn,
                  rjcRepeatHeight = rjcRepeatHeight rjc + nRepeats * deltaH
                }
            )
          return (traceIt ("NNN " ++ show n ++ " ") (n - nRepeats * deltaN))
    else do
      put rjc {rjcPrevJetNumber = jn}
      return n

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
    Just y' -> dropRockAt r' y'
    Nothing -> do
      rjc' <- get
      let c' = fromJust $ addRock r' y c
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

getCave :: State RJC Cave
getCave = do
  rjc <- get
  return (rjcCave rjc)

peekJetNumber :: State RJC Int
peekJetNumber = do
  s <- get
  let ((n, _) : _) = rjcJets s
  return n

nextJet :: State RJC Jet
nextJet = do
  s <- get
  let ((_, j) : js) = rjcJets s
  put (s {rjcJets = js})
  return j

peekRockNumber :: State RJC Int
peekRockNumber = do
  s <- get
  let ((n, _) : _) = rjcRocks s
  return n

nextRock :: State RJC Rock
nextRock = do
  s <- get
  let ((_, r) : rs) = rjcRocks s
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
