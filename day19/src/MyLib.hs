module MyLib
  ( Resource (..),
    Thing (..),
    Node (..),
    nodeHowMany,
    Recipe,
    makeRecipe,
    findMaxima,
    bestPossibleScore,
    timeUntil,
    timeUntilN,
    nodeSuccessors,
    removeResources,
    runRobots,
  )
where

import Control.Monad
import qualified Data.Map.Strict as M
import Data.Maybe
import Debug.Trace
import Safe.Foldable

-- | Resources
data Resource
  = Ore
  | Clay
  | Obsidian
  | Geode
  deriving (Eq, Ord, Read, Show)

allResources :: [Resource]
allResources = [Ore, Clay, Obsidian, Geode]

-- | The resources and the robots used to create them
--
-- These are used as goals when asking things like "how long until I could have
-- an ore robot?" or "how long until I could have a geode?"
data Thing
  = Res Resource
  | Robot Resource
  deriving (Eq, Ord, Show)

-- | A recipe for creating robots.
--
-- Maps from a (Robot r) to the (n, Thing) pairs needed to make one.
data Recipe = Recipe (M.Map Thing [(Int, Thing)]) (M.Map Thing Int) deriving (Eq, Ord, Show)

makeRecipe :: M.Map Thing [(Int, Thing)] -> Recipe
makeRecipe ingredients = Recipe ingredients (findMaxima ingredients)

recipeIngredients :: Recipe -> M.Map Thing [(Int, Thing)]
recipeIngredients (Recipe m _) = m

recipeInputs :: Recipe -> Thing -> [(Int, Thing)]
recipeInputs (Recipe m _) t = fromJust . M.lookup t $ m

recipeMaxNeeded :: Recipe -> Thing -> Int
recipeMaxNeeded (Recipe _ m) t = fromJust . M.lookup t $ m

-- | Computes the maximum number of each type of robot needed.
findMaxima :: M.Map Thing [(Int, Thing)] -> M.Map Thing Int
findMaxima m =
  M.fromList
    [ (Robot r, maxNeeded)
      | r <- allResources,
        r /= Geode,
        let maxNeeded = fromMaybe 0 . maximumMay . map fst . filter (isRes r . snd) . concat . M.elems $ m
    ]
  where
    isRes r x = x == Res r

-- | What do you need to get a thing?
precursors :: Recipe -> Thing -> [(Int, Thing)]
precursors r t =
  case t of
    -- To make a resource, you need one robot for that resource.
    (Res x) -> [(1, Robot x)]
    -- To make a robot, you need whatever the recipe specifies.
    -- We expect a recipe to have something for every robot type.
    (Robot _) -> recipeInputs r t

-- | A node in the search tree has counts for resources and robots.
data Node = Node
  { -- how many of each resource and robot type we have
    nodeCounts :: M.Map Thing Int,
    -- how much time is remaining
    nodeTimeLeft :: Int
  }
  deriving (Eq, Ord)

instance Show Node where
  show node = timeLeft ++ ":" ++ resources ++ ":" ++ robots
    where
      timeLeft = show . nodeTimeLeft $ node
      resources = makeCounts Res
      robots = makeCounts Robot
      makeCounts kind = show [nodeHowMany node (kind r) | r <- allResources]

-- | How many of the given thing are there?
nodeHowMany :: Node -> Thing -> Int
nodeHowMany n t = fromMaybe 0 . M.lookup t $ nodeCounts n

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

-- | What are all of the successors of a search node?
--
-- The choice is what kind of robot to make, if any.  We're allowed
-- to create just one robot in each time interval.  Sometimes, though
-- we might want to skip making one so we have the resources to make
-- something else later.
--
-- The sequencing is:
--   (1) take out resources needed to make new robot
--   (2) add resources based on existing robots
--   (3) add new robot
--   (4) advance the clock
--
-- The "wantToAdd" check is from a clue I found on Reddit.  There's no
-- need to have more robots for a resource that the biggest number for
-- that resource in recipes.
nodeSuccessors :: Recipe -> Node -> [Node]
nodeSuccessors recipe step0 =
  [ step4
    | ((nToAdd, tToAdd), resourcesNeeded) <- possibleRecipes recipe,
      wantToAdd recipe tToAdd (nodeHowMany step0 tToAdd),
      step1 <- maybeToList (removeResources step0 resourcesNeeded),
      let step2 = runRobots step1,
      let step3 = nodeAdd nToAdd tToAdd step2,
      let step4 = step3 {nodeTimeLeft = nodeTimeLeft step3 - 1}
  ]

wantToAdd :: Recipe -> Thing -> Int -> Bool
wantToAdd _ (Robot Geode) _ = True
wantToAdd recipe t@(Robot _) alreadyHave = alreadyHave < recipeMaxNeeded recipe t
wantToAdd _ _ _ = False

possibleRecipes :: Recipe -> [((Int, Thing), [(Int, Thing)])]
possibleRecipes r =
  ((0, Robot Ore), [])
    : [ ((1, t), inputs)
        | (t, inputs) <- M.toList (recipeIngredients r)
      ]

removeResources :: Node -> [(Int, Thing)] -> Maybe Node
removeResources =
  foldM removeOne
  where
    removeOne a (n, t) =
      if n <= nBefore
        then Just $ nodeAdd (negate n) t a
        else Nothing
      where
        nBefore = nodeHowMany a t

runRobots :: Node -> Node
runRobots node =
  foldl runOne node (M.toList . nodeCounts $ node)
  where
    runOne node0 (t, n) =
      case t of
        Res _ -> node0
        Robot x -> nodeAdd n (Res x) node0

nodeAdd :: Int -> Thing -> Node -> Node
nodeAdd 0 _ node = node
nodeAdd n t node =
  node {nodeCounts = M.insert t (n0 + n) counts0}
  where
    n0 = fromMaybe 0 . M.lookup t $ counts0
    counts0 = nodeCounts node

traceIt :: Show a => [Char] -> a -> a
traceIt lbl x = trace (lbl ++ " " ++ show x) x
