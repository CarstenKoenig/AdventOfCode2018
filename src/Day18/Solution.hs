{-# LANGUAGE TupleSections #-}
module Day18.Solution
  ( run
  , showArea
  ) where


import qualified Data.Array.IArray as Arr
import           Data.Array.Unboxed (UArray)
import           Data.Ix (inRange)
import qualified Data.Map.Strict as M


type Generations = [State]


data State = State
  { area       :: !Area
  , areaBounds :: !Bounds
  , areaCoords :: [Coord]
  }


type Area = UArray Coord Acre


type Acre = Char


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
    acres       = Arr.elems start
    trees       = length $ filter (== '|') acres
    lumberyards = length $ filter (== '#') acres


----------------------------------------------------------------------
-- part 2

-- | calculates the 1_000_000_000 generations score
-- finds a cylce and uses this to simplify
part2 :: Generations -> Int
part2 gens =
  let (lastIter, firstIter) = findCycle gens
      lookAtIter = firstIter + ((target - firstIter) `mod` (lastIter - firstIter))
  in scoreAfterIterations gens lookAtIter
  where
    target = 1000000000


-- | looks for a cylce after the first 500 generations
findCycle :: Generations -> (Int, Int)
findCycle gens =
  go M.empty $ drop 400 $ zip [0..] $ map area gens
  where
    go _ [] = error "end of infinite sequence!?"
    go seen ((i,a):as) =
      case M.lookup a seen of
        Nothing ->
          go (M.insert a i seen) as
        Just i' -> (i,i')


----------------------------------------------------------------------
-- Generations

-- | a infinte list of generations starting at the given state
generations :: State -> Generations
generations = iterate step


-- | calcualtes the next iteration using 'nextAcre'
step :: State -> State
step state = state { area = area' }
  where area' = Arr.array (areaBounds state) . map (nextAcre state) $ (areaCoords state)


-- | calculate the content for agiven coord according to the rules given in todays problem
nextAcre :: State -> Coord -> (Coord, Acre)
nextAcre state coord =
  let next =
        case area state Arr.! coord of
          '.' -> if trees >= 3 then '|' else '.'
          '|' -> if lumberyards >= 3 then '#' else '|'
          '#' -> if lumberyards >= 1 && trees >= 1 then '#' else '.'
          c   -> error $ "saw unknown Acre at " ++ show coord ++ ": " ++ show c
  in (coord, next)
  where
    surrs       = surroundingAcres state coord
    trees       = length $ filter (== '|') surrs
    lumberyards = length $ filter (== '#') surrs


----------------------------------------------------------------------
-- helpers

-- | gives a list of all the neighbor contents of a coord
surroundingAcres :: State -> Coord -> [Acre]
surroundingAcres state = map (area state Arr.!) . surroundings (areaBounds state)


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
    crds = Arr.indices a


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
    getAcreChar r c = a Arr.! (r,c)


----------------------------------------------------------------------
-- parsing

parseArea :: String -> Area
parseArea = Arr.array ((0,0), (49,49)). parseRows


parseRows :: String -> [(Coord, Acre)]
parseRows = concat . zipWith parseRow [0..] . lines


parseRow :: Int -> String -> [(Coord, Acre)]
parseRow r = zipWith (\col ch -> ((r,col), ch)) [0..]
