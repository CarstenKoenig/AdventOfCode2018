{-# LANGUAGE DeriveGeneric #-}
module Day22.Solution
  ( run
  , Input (..)
  , example
  , drawMap
  ) where


import           Data.Hashable (Hashable)
import           Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import           GHC.Generics (Generic)
import qualified Utils.Astar as Astar


-- | (X,Y
type Coord = (Int, Int)

data RegionType
  = Rocky
  | Narrow
  | Wet
  deriving (Show, Eq, Ord, Enum, Bounded, Generic)

instance Hashable RegionType


type Depth = Int

data Input = Input
  { inputDepth  :: Depth
  , inputTarget :: Coord
  , regionMap   :: Map Coord RegionType
  }


input :: Input
input = makeInput 5913 (8,701)


makeInput :: Int -> Coord -> Input
makeInput d tgt = Input d tgt $ buildRegionMap (d, tgt) (1100,1100)


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


regionType :: Input -> Coord -> RegionType
regionType inp c =
  case Map.lookup c (regionMap inp) of
    Just res -> res
    Nothing  -> error $ "not in map: " ++ show c


buildRegionMap :: (Int, Coord) -> Coord -> Map Coord RegionType
buildRegionMap (depth,tgt) (maxX, maxY) = Map.map calcRegionType erosionMap
  where
    calcRegionType :: Int -> RegionType
    calcRegionType erosionLvl =
      case erosionLvl `mod` 3 of
        0 -> Rocky
        1 -> Wet
        _ -> Narrow
    erosionMap = Map.fromList [ (c, erosionLevel c) | x <- [0..maxX], y <- [0..maxY], let c = (x,y) ]
    erosionLevel  (x,y) = (geoIndex (x,y) + depth) `mod` 20183
    geoIndex (0,0)                    = 0
    geoIndex !c | c == tgt            = 0
    geoIndex (!x,0)                   = x * 16807
    geoIndex (0,!y)                   = y * 48271
    geoIndex (!x,!y)                  = erosionMap Map.! (x-1,y) * erosionMap Map.! (x,y-1)



example :: Input
example = makeInput 510 (10,10)


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


run :: IO ()
run = do
  putStrLn "DAY 22"
  let inp = input

  putStrLn $ "part 1: " ++ show (riskLevel inp)
  putStrLn $ "part 2: " ++ show (totalTime $ findPath inp)


data State = State
  { pos    :: Coord
  , tool   :: Tool
  } deriving (Show, Eq, Ord, Generic)


instance Hashable State


data Tool = Neither | Torch | ClimbingGear
  deriving (Show, Eq, Ord, Generic)

instance Hashable Tool


totalTime :: [State] -> Int
totalTime path = sum $ zipWith time path (tail path)


time :: State -> State -> Int
time (State fromPos fromTool) (State toPos toTool) =
      (if fromPos == toPos then 0 else 1)  + (if fromTool == toTool then 0 else 7)


findPath :: Input -> [State]
findPath inp = Astar.aStar (astarParams inp) start


possibleTools :: RegionType -> [Tool]
possibleTools Rocky  = [ClimbingGear, Torch]
possibleTools Wet    = [ClimbingGear, Neither]
possibleTools Narrow = [Torch, Neither]


isPossibleTool :: RegionType -> Tool -> Bool
isPossibleTool regType = (`elem` possibleTools regType)


adjacent :: Coord -> [Coord]
adjacent (x,y) = [ c | c@(x',y') <- [(x,y+1), (x+1,y), (x-1,y), (x,y-1)], x' >= 0, y' >= 0 ]


neighbors :: Input -> State -> [State]
neighbors inp (State curPos curTool) =
  [ State newPos newTool
  | newPos <- adjacent curPos ++ [curPos] -- may stay in place too
  , let newRegion = regionType inp newPos
  , newTool <- [ t | t <- possibleTools newRegion, isPossibleTool curRegion t ]
  , (newPos, newTool) /= (curPos, curTool)
  ]
  where
    curRegion = regionType inp curPos


start :: State
start = State (0,0) Torch

astarParams :: Input -> Astar.Parameter State State
astarParams inp = Astar.Parameter
  (heuristic inp)
  (neighbors inp)
  (isGoal inp)
  id
  time

heuristic :: Input -> State -> Int
heuristic inp (State curPos _ ) = dist (inputTarget inp) curPos
  where dist (x,y) (x',y') = abs (x'-x) + abs (y'-y)

isGoal :: Input -> State -> Bool
isGoal inp (State p Torch ) = p == inputTarget inp
isGoal _   _                = False
