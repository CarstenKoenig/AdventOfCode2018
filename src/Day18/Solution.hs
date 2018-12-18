{-# LANGUAGE TupleSections #-}
module Day18.Solution where

import           Data.Char (isDigit)
import           Data.Ix (inRange)
import           Data.List (foldl')
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Text.Parsec


size :: Int
size = 49


target :: Int
target = 1000000000


run :: IO ()
run = do
  putStrLn "DAY 18"
  inp <- inputTxt
  putStrLn $ show (part2 524 inp)
  putStrLn $ show (part2 (528+24) inp)
  putStrLn $ show $ findCycle inp


data Acre = Ground | Tree | LumberYard
  deriving (Eq, Show, Ord, Enum, Bounded)


type Area = Map Coord Acre


-- | row/col
type Coord = (Int, Int)


part2 :: Int -> Area -> Int
part2 at start =
  let end = iteration at start
      trees = length . filter (== Tree) $ M.elems end
      lumberyards = length . filter (== LumberYard) $ M.elems end
  in trees * lumberyards


findCycle :: Area -> (Int, Int, Int)
findCycle area =
  go M.empty $ drop 500 $ zip [0..] (iterate step area)
  where
    go :: Map Int Int -> [(Int, Area)] -> (Int, Int, Int)
    go seen ((i,a):as) =
      case M.lookup sc seen of
        Nothing ->
          go (M.insert sc i seen) as
        Just i' -> (i,i',sc)
      where sc = score a



iteratePart1 :: Int -> Area -> IO ()
iteratePart1 n start =
  mapM_ putStrLn $ (map (\(i,a) -> show (i, score a)) . take n $ zip [0..] $ iterate step start)


part1 :: Area -> Int
part1 start =
  let end = iteration 10 start
      trees = length . filter (== Tree) $ M.elems end
      lumberyards = length . filter (== LumberYard) $ M.elems end
  in trees * lumberyards


score :: Area -> Int
score start =
  let trees = length . filter (== Tree) $ M.elems start
      lumberyards = length . filter (== LumberYard) $ M.elems start
  in trees * lumberyards


iteration :: Int -> Area -> Area
iteration n start = (iterate step start) !! n


step :: Area -> Area
step area =
  M.fromList . map (nextAcre area) $ coords


nextAcre :: Area -> Coord -> (Coord, Acre)
nextAcre area coord =
  let next =
        case area M.! coord of
          Ground -> if trees >= 3 then Tree else Ground
          Tree   -> if lumberyards >= 3 then LumberYard else Tree
          LumberYard -> if lumberyards >= 1 && trees >= 1 then LumberYard else Ground
  in (coord, next)
  where
    trees = surroundingAcresOf Tree area coord
    lumberyards = surroundingAcresOf LumberYard area coord


surroundingAcresOf :: Acre -> Area -> Coord -> Int
surroundingAcresOf acre area =
  length . filter (== acre) . surroundingAcres area


surroundingAcres :: Area -> Coord -> [Acre]
surroundingAcres area = map (area M.!) . surroundings


surroundings :: Coord -> [Coord]
surroundings (r,c) =
  [ coord | dr <- [-1..1], dc <- [-1..1], let coord = (r+dr,c+dc), inRange bounds coord , (dr,dc) /= (0,0) ]


bounds :: (Coord, Coord)
bounds = ((0,0), (size,size))


coords :: [Coord]
coords = [(r,c) | r <- [0..size], c <- [0..size]]

----------------------------------------------------------------------
-- IO
inputTxt :: IO Area
inputTxt = parseArea <$> readFile "./src/Day18/input.txt"


inputTst :: IO Area
inputTst = parseArea <$> readFile "./src/Day18/test.txt"

showArea :: Area -> IO ()
showArea area =
  mapM_ showLine [0..size]
  where
    showLine r = putStrLn $ map (getAcreChar r) [0..size]
    getAcreChar r c =
      case M.lookup (r,c) area of
        Just Ground     -> '.'
        Just Tree       -> '|'
        Just LumberYard -> '#'
        Nothing         -> '?'


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
