module Main where

import qualified Data.Set as S
import Debug.Trace
import Linear.V2 (V2 (..))
import MyLib

main :: IO ()
main = do
  runInput "test.txt"
  runInput "input.txt"

runInput :: String -> IO ()
runInput fileName = do
  putStrLn fileName
  rockText <- readFile "rocks.txt"
  let rocks = take 2022 . cycle . parseRocks $ rockText
  input <- readFile fileName
  let jets = cycle $ parseJets input
  let initialState = State {stateCave = initialCave, stateRocks = rocks, stateJets = jets}
  let finalState = run initialState
  print . caveHeight . stateCave $ finalState

-- | Run until there are no more rocks
run :: State -> State
run s0 =
  case nextRock s0 of
    Nothing -> s0
    Just (r, s1) ->
      run (dropRock r (startingPointForNextRock (stateCave s1)) s1)

traceIt :: Show a => [Char] -> a -> a
traceIt lbl x = trace (lbl ++ " " ++ show x) x

-- | Drop one rock from the given point:
dropRock :: Rock -> Point -> State -> State
dropRock r p0 s0 =
  let (j, s1) = nextJet s0
      c = stateCave s1
      p1 = applyJet c r p0 j
   in case applyGravity c r p1 of
        Nothing -> s1 {stateCave = addRock c r p1}
        Just p2 -> dropRock r p2 s1

-- | Where does the next rock start falling?
startingPointForNextRock :: Cave -> Point
startingPointForNextRock c =
  V2 2 (maxY + 4)
  where
    maxY = caveHeight c

-- | The state of the computation is the configuration of the cave, and the sequence of rocks and jets.
data State = State
  { stateCave :: Cave,
    stateRocks :: [Rock],
    stateJets :: [Dir]
  }

instance Show State where
  show s = "State{rockCount=" ++ show (length (stateRocks s)) ++ "}"

-- | Returns the next rock to fall, and an updated state.
nextRock :: State -> Maybe (Rock, State)
nextRock s =
  case stateRocks s of
    [] -> Nothing
    r : rs -> Just (r, s {stateRocks = rs})

-- | Returns the next jet direction, and an updated state.
nextJet :: State -> (Dir, State)
nextJet s =
  case stateJets s of
    d : ds -> (d, s {stateJets = ds})
    _ -> error "ran out of jets"