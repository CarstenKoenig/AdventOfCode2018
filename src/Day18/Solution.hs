{-# LANGUAGE TupleSections #-}
module Day18.Solution
  ( run
  , showArea
  ) where


import           Data.Ix (inRange)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M


type Generations = [State]


data State = State
  { area       :: !Area
  , areaBounds :: !Bounds
  , areaCoords :: [Coord]
  }


type Area = Map Coord Acre


data Acre = Ground | Tree | LumberYard
  deriving (Eq, Show, Ord, Enum, Bounded)


type Bounds = (Coord, Coord)


-- | row/col
type Coord = (Int, Int)


----------------------------------------------------------------------
-- main

run :: IO ()
run = do
  putStrLn "DAY 18"
  input <- inputTxt
  let gens = generations input

  putStrLn $ "part 1: " ++ show (part1 gens)
  putStrLn $ "part 2: " ++ show (part2 gens)


----------------------------------------------------------------------
-- part 1

-- | just looks for teh 'scoreAfterIterations' for 'n' = 10
part1 :: Generations -> Int
part1 gens =
  scoreAfterIterations gens 10


-- | finds the 'score' for the 'n' generation
scoreAfterIterations :: Generations -> Int -> Int
scoreAfterIterations gens n = score . area $ gens !! n


-- | calculates the score for an area (= number of Trees * number of LumberYards)
score :: Area -> Int
score start = trees * lumberyards
  where
    acres       = M.elems start
    trees       = length $ filter (== Tree) acres
    lumberyards = length $ filter (== LumberYard) acres


----------------------------------------------------------------------
-- part 2

-- | calculates the 1_000_000_000 generations score
-- finds a cylce and uses this to simplify
part2 :: Generations -> Int
part2 gens =
  let (lastIter, firstIter, _) = findCycle gens
      lookAtIter = firstIter + ((target - firstIter) `mod` (lastIter - firstIter))
  in scoreAfterIterations gens lookAtIter
  where
    target = 1000000000


-- | looks for a cylce after the first 500 generations
findCycle :: Generations -> (Int, Int, Int)
findCycle gens =
  go M.empty $ drop 500 $ zip [0..] $ map area gens
  where
    go _ [] = error "end of infinite sequence!?"
    go seen ((i,a):as) =
      case M.lookup sc seen of
        Nothing ->
          go (M.insert sc i seen) as
        Just i' -> (i,i',sc)
      where sc = score a


----------------------------------------------------------------------
-- Generations

-- | a infinte list of generations starting at the given state
generations :: State -> Generations
generations = iterate step


-- | calcualtes the next iteration using 'nextAcre'
step :: State -> State
step state = state { area = area' }
  where area' = M.fromList . map (nextAcre state) $ (areaCoords state)


-- | calculate the content for agiven coord according to the rules given in todays problem
nextAcre :: State -> Coord -> (Coord, Acre)
nextAcre state coord =
  let next =
        case area state M.! coord of
          Ground     -> if trees >= 3 then Tree else Ground
          Tree       -> if lumberyards >= 3 then LumberYard else Tree
          LumberYard -> if lumberyards >= 1 && trees >= 1 then LumberYard else Ground
  in (coord, next)
  where
    surrs       = surroundingAcres state coord
    trees       = length $ filter (== Tree) surrs
    lumberyards = length $ filter (== LumberYard) surrs


----------------------------------------------------------------------
-- helpers

-- | gives a list of all the neighbor contents of a coord
surroundingAcres :: State -> Coord -> [Acre]
surroundingAcres state = map (area state M.!) . surroundings (areaBounds state)


-- | all 8 neighbors of a coords which are in the bounds
surroundings :: Bounds -> Coord -> [Coord]
surroundings bds (r,c) =
  [ coord | dr <- [-1..1], dc <- [-1..1], let coord = (r+dr,c+dc), (dr,dc) /= (0,0), valid coord ]
  where valid = inRange bds


-- | calculates Bounds from Area
-- looks for minimum/maximum row/col
bounds :: Area -> Bounds
bounds a =
  ((minRow,minCol), (maxRow,maxCol))
  where
    minRow = minimum $ map fst $ crds
    maxRow = maximum $ map fst $ crds
    minCol = minimum $ map snd $ crds
    maxCol = maximum $ map snd $ crds
    crds = M.keys a


-- | all Coordinates in Bounds
coords :: Bounds -> [Coord]
coords bds = [(r,c) | r <- [minRow..maxRow], c <- [minCol..maxCol]]
  where ((minRow,minCol), (maxRow, maxCol)) = bds


----------------------------------------------------------------------
-- IO

inputTxt :: IO State
inputTxt = do
  a <- parseArea <$> readFile "./src/Day18/input.txt"
  let bds = bounds a
  let crds = coords bds
  pure $ State a bds crds


showArea :: State -> IO ()
showArea (State a bds _) =
  mapM_ showLine [minRow..maxRow]
  where
    ((minRow,minCol), (maxRow,maxCol)) = bds
    showLine r = putStrLn $ map (getAcreChar r) [minCol..maxCol]
    getAcreChar r c =
      case M.lookup (r,c) a of
        Just Ground     -> '.'
        Just Tree       -> '|'
        Just LumberYard -> '#'
        Nothing         -> '?'


----------------------------------------------------------------------
-- parsing

parseArea :: String -> Area
parseArea = M.fromList . parseRows


parseRows :: String -> [(Coord, Acre)]
parseRows = concat . zipWith parseRow [0..] . lines


parseRow :: Int -> String -> [(Coord, Acre)]
parseRow r = zipWith (\col ch -> ((r,col), toAcre ch)) [0..]
  where toAcre '.' = Ground
        toAcre '|' = Tree
        toAcre '#' = LumberYard
        toAcre c   = error $ "unkown area-type " ++ show c
