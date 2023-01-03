{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main where

import Control.Monad.State
  ( MonadState (get, put),
    State,
    execState,
  )
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import MyLib
  ( Cave,
    Jet (..),
    Rock,
    addRock,
    moveRockLeft,
    moveRockRight,
    parseJets,
    parseRocks,
  )

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
  print $ runWithCount rocks jets 2022
  print $ runWithCount rocks jets 1000000000000

runWithCount :: [Rock] -> [Jet] -> RockCount -> CaveHeight
runWithCount rocks jets n =
  rjcRepeatHeight rjc + caveHeight (rjcCave rjc) - 1
  where
    rjc = execState (dropNRocks n) (initialRJC rocks jets)

type CaveHeight = Integer

caveHeight :: Cave -> CaveHeight
caveHeight = toInteger . length

type RockCount = Integer

-- | how much of the cave do we save when saving checkpoints for detecting repeats
--
-- If a rock falls farther than this before landing, then our repeat detection
-- may have false positives.
caveSizeForCaching :: Int
caveSizeForCaching = 40

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
    -- used to detect repeats
    rjcCheckpoints :: M.Map (Int, Int, Cave) (RockCount, CaveHeight),
    -- height from repeats, not included in actual cave because we didn't actually do them.
    -- when a repeat is removed, its height is added here
    rjcRepeatHeight :: CaveHeight
  }

rjcPeekJetNumber :: RJC -> Int
rjcPeekJetNumber = fst . head . rjcJets

rjcPeekRockNumber :: RJC -> Int
rjcPeekRockNumber = fst . head . rjcRocks

dropNRocks :: RockCount -> State RJC ()
dropNRocks n = do
  n' <- checkForRepeat n
  if n' == 0
    then return ()
    else do
      dropRock
      dropNRocks (n' - 1)

checkForRepeat :: RockCount -> State RJC RockCount
checkForRepeat n = do
  rjc <- get
  let jn = rjcPeekJetNumber rjc
  if jn < rjcPrevJetNumber rjc
    then do
      let c = rjcCave rjc
      let rn = rjcPeekRockNumber rjc
      let top = take caveSizeForCaching c
      let h = caveHeight c
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
          let deltaH = h - prevHeight
          let deltaN = prevN - n
          let nRepeats = n `div` deltaN
          put
            ( rjc
                { rjcCheckpoints = M.empty,
                  rjcPrevJetNumber = jn,
                  rjcRepeatHeight = rjcRepeatHeight rjc + toInteger nRepeats * deltaH
                }
            )
          return (n - nRepeats * deltaN)
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
    Just y' ->
      if negate y' < caveSizeForCaching
        then dropRockAt r' y'
        else error "rock fell too far"
    Nothing -> do
      rjc' <- get
      let c' = fromJust $ addRock r' y c
      put rjc' {rjcCave = c'}
      return ()

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
