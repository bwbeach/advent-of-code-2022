module MyLib
  ( Resource (..),
    Thing (..),
    Node (..),
    Recipe (..),
    bestPossibleScore,
    timeUntil,
    timeUntilN,
  )
where

import qualified Data.Map.Strict as M
import Data.Maybe
import Debug.Trace

-- | Resources
data Resource
  = Ore
  | Clay
  | Obsidian
  | Geode
  deriving (Eq, Ord, Read, Show)

-- | The resources and the robots used to create them
data Thing
  = Res Resource
  | Robot Resource
  deriving (Eq, Ord, Show)

-- | A node in the search tree has counts for resources and robots.
data Node = Node
  { -- how many of each resource and robot type we have
    nodeCounts :: M.Map Thing Int,
    -- how much time is remaining
    nodeTimeLeft :: Int
  }
  deriving (Eq, Ord, Show)

-- | How many of the given thing are there?
nodeHowMany :: Node -> Thing -> Int
nodeHowMany n t = fromMaybe 0 . M.lookup t $ nodeCounts n

-- | A recipe for creating robots.
--
-- Maps from a robot to the (n, resource) pairs needed to make one.
newtype Recipe = Recipe (M.Map Thing [(Int, Thing)]) deriving (Eq, Ord, Show)

-- | What do you need to get a thing?
precursors :: Recipe -> Thing -> [(Int, Thing)]
precursors _ (Res x) = [(1, Robot x)]
precursors (Recipe r) t@(Robot _) = fromMaybe [] . M.lookup t $ r

-- | What's the best possible score that could come from this Node?
bestPossibleScore :: Recipe -> Node -> Int
bestPossibleScore recipe node =
  ng + ngr * t + tgr * (tgr + 1) `div` 2
  where
    t = nodeTimeLeft node
    -- nuber of geodes we already have
    ng = nodeHowMany node (Res Geode)
    -- number of geode robots
    ngr = nodeHowMany node (Robot Geode)
    -- minimum time until there is one more geode robot
    dt = max 1 (timeUntil recipe node (Robot Geode))
    -- how many time units do we run with additional geode robots
    -- at soonest, this can start 1 time unite from now
    tgr = max 0 (t - dt)

-- | How long until we might possibly have one of the given thing?
timeUntil :: Recipe -> Node -> Thing -> Int
timeUntil recipe node =
  go 1
  where
    -- How long until there might possibly be n of the given thing?
    go n t =
      if 0 < nodeHowMany node t
        then -- we already have one.  TODO: code to estimate how to get to n
          0
        else -- we need all of the precursers, then time to build to n of this thing
          maximum (map (uncurry go) (precursors recipe t)) + timeUntilN n

-- | What's the absolute minimum time until we could have N of something...
-- starting from the point where we have the robot that is about to make the
-- first one.  Nothing could be better than adding one more of those robots
-- every time interval, so that's the assumption for the best case.
-- Given that, the number of the resource at time T is  t(t+1)/2
--
-- The answer is the positive solution to this quadratic, rounded up:
--
--       t(t+1)/2 = n
--       0.5 t^2 + 0.5 t - n = 0
timeUntilN :: Int -> Int
timeUntilN n =
  ceiling ((negate b + sqrt (b * b - 4 * a * c)) / (2 * a))
  where
    a = 0.5 :: Double
    b = 0.5
    c = fromIntegral (negate n)

traceIt :: Show a => [Char] -> a -> a
traceIt lbl x = trace (lbl ++ " " ++ show x) x