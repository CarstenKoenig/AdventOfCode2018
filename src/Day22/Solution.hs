module Day22.Solution
  ( run
  , Input (..)
  , example
  , drawMap
  ) where


import Algorithm.Search (aStar)
import Data.Array (Array, (!), array)


-- | (X,Y)
type Coord = (Int, Int)


data RegionType
  = Rocky
  | Narrow
  | Wet


type Depth = Int


-- | input-type for the problem
-- includes the parametes and a lazy-map from 'Coord' to 'RegionType'
data Input = Input
  { inputDepth  :: !Depth
  , inputTarget :: !Coord
  , regionMap   :: Array Coord RegionType
  }


-- | State for part2 / 'findPath'
data State = State
  { pos    :: !Coord
  , tool   :: !Tool
  } deriving (Show, Ord, Eq)


data Tool = Neither | Torch | ClimbingGear
  deriving (Show, Ord, Eq)


----------------------------------------------------------------------
-- main

run :: IO ()
run = do
  putStrLn "DAY 22"
  let inp = input

  putStrLn $ "part 1: " ++ show (riskLevel inp)
  putStrLn $ "part 2: " ++ show (fst <$> findPath inp)


----------------------------------------------------------------------
-- inputs

input :: Input
input = makeInput 5913 (8,701)


example :: Input
example = makeInput 510 (10,10)


makeInput :: Depth -> Coord -> Input
makeInput d tgt = Input d tgt $ buildRegionMap (d, tgt) (500, 800)


----------------------------------------------------------------------
-- algorithms

-- | calculates teh risk-level
-- as described in the problem
riskLevel :: Input -> Int
riskLevel inp =
  sum $ map riskLevelOf $ [(x,y) | x <- [0..toX], y <- [0..toY]]
  where
    riskLevelOf (x,y) =
      case regionType inp (x,y) of
        Rocky  -> 0
        Wet    -> 1
        Narrow -> 2
    (toX,toY) = inputTarget inp



-- | we start at (0,0) equiped with a Torch for part 2
start :: State
start = State (0,0) Torch


-- | calculates the time between two states
-- every movement takes 1 Minute - every change of tools 7
time :: State -> State -> Int
time (State fromPos fromTool) (State toPos toTool) =
      (if fromPos == toPos then 0 else 1)  + (if fromTool == toTool then 0 else 7)


-- | finds the optimal path to the target-coord using an A* algorithm
findPath :: Input -> Maybe (Int, [State])
findPath inp = aStar (neighbors inp) time (heuristic inp) (isGoal inp) start


-- | given a region-type: what tools are equipable there
possibleTools :: RegionType -> [Tool]
possibleTools Rocky  = [ClimbingGear, Torch]
possibleTools Wet    = [ClimbingGear, Neither]
possibleTools Narrow = [Torch, Neither]


-- | checks if a tool is equipable for a region-type
isPossibleTool :: RegionType -> Tool -> Bool
isPossibleTool regType = (`elem` possibleTools regType)


-- | gives all adjacent corods - negative coords are all solid walls so we skip those
adjacent :: Coord -> [Coord]
adjacent (x,y) = [ c | c@(x',y') <- [(x,y+1), (x+1,y), (x-1,y), (x,y-1)], x' >= 0, y' >= 0 ]


-- | calculates the neighbor states from a given
-- including: movement to adjacent positions and changing of tools
neighbors :: Input -> State -> [State]
neighbors inp (State curPos curTool) =
  [ State newPos newTool
  | newPos <- adjacent curPos
  , let newRegion = regionType inp newPos
  , newTool <- [ t | t <- possibleTools newRegion, isPossibleTool curRegion t ]
  , (newPos, newTool) /= (curPos, curTool)
  ]
  where
    curRegion = regionType inp curPos


-- | as every step takes 1 Minute we can the
-- manhatten-distance as an heuristic for A*
heuristic :: Input -> State -> Int
heuristic inp (State curPos _ ) = dist (inputTarget inp) curPos
  where dist (x,y) (x',y') = abs (x'-x) + abs (y'-y)


-- | the goal is to reach the target-coord equiped with a torch
isGoal :: Input -> State -> Bool
isGoal inp (State p Torch ) = p == inputTarget inp
isGoal _   _                = False


----------------------------------------------------------------------
-- calculation of the region-type
-- we need to memoize for this
-- here this is done using a lazy 'Array'

-- | get's a coords region type using
-- the memoized map in 'Input'
regionType :: Input -> Coord -> RegionType
regionType inp c = regionMap inp ! c


-- | builds an lazy-array with the problems RegionTypes
buildRegionMap :: (Int, Coord) -> Coord -> Array Coord RegionType
buildRegionMap (depth,tgt) (maxX, maxY) = fmap calcRegionType erosionMap
  where
    calcRegionType :: Int -> RegionType
    calcRegionType erosionLvl =
      case erosionLvl `mod` 3 of
        0 -> Rocky
        1 -> Wet
        _ -> Narrow
    erosionMap = array ((0,0), (maxX,maxY)) [ (c, erosionLevel c) | x <- [0..maxX], y <- [0..maxY], let c = (x,y) ]
    erosionLevel  (x,y) = (geoIndex (x,y) + depth) `mod` 20183
    geoIndex (0,0)                    = 0
    geoIndex !c | c == tgt            = 0
    geoIndex (!x,0)                   = x * 16807
    geoIndex (0,!y)                   = y * 48271
    geoIndex (!x,!y)                  = erosionMap ! (x-1,y) * erosionMap ! (x,y-1)


----------------------------------------------------------------------
-- IO

-- | in case you want to see the map
drawMap :: Input -> (Int, Int) -> IO ()
drawMap inp (width, height) =
  mapM_ drawLine [0..height]
  where
    drawLine y = putStrLn $ map (formatRegion y) [0..width]
    formatRegion y x =
      case regionType inp (x,y) of
        Rocky  -> '.'
        Wet    -> '='
        Narrow -> '|'
