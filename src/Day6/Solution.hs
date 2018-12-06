{-# LANGUAGE TupleSections #-}
module Day6.Solution where

import           Data.Char (isDigit)
import           Data.List (sortOn)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import           Text.Parsec
import qualified Utils.Counter as C

-- | a point has a unique number and a coordinate
data Point = Point
  { pointNr    :: PointNr
  , pointCoord :: Coord
  }
  deriving Show

type PointNr = Int

-- | coordinate = pair of x and y integers
type Coord = (Int, Int)

-- | Boundary of the problem-grid
-- pair of top-left and bottom-right coordinate
-- of a enclosing rectangle
type Boundary = (Coord, Coord)


-- | Grid for part 1
-- maps coords to their nearest point
-- nearest point with this point, if it does not
-- lie in an infinite area
type Grid = Map Coord PointNr


run :: IO ()
run = do
  putStrLn "DAY 6"

  inp <- inputTxt
  let bound = findBounds inp

  putStrLn $ "part 1: " ++ show (part1 bound inp)
  putStrLn $ "part 2: " ++ show (part2 bound inp)


----------------------------------------------------------------------
-- Part 1

part1 :: Boundary -> [Point] -> Int
part1 bound = findLargestArea . grid bound


----------------------------------------------------------------------
-- Part 2

part2 :: Boundary -> [Point] -> Int
part2 = coordsNearAll 10


----------------------------------------------------------------------
-- Algorithm


-- | just count all Coordinates that are @nearAll points
coordsNearAll :: Int -> Boundary -> [Point] -> Int
coordsNearAll slag ((x0,y0), (x1,y1)) crds =
  length . filter (nearAll crds) $
  [ (x,y) | x <- [x0-slag..x1+slag], y <- [y0-slag..y1+slag]]


-- | the puzzle says a coord is near all points if the
-- sum of their distances to that coord is less than 10.000
nearAll :: [Point] -> Coord -> Bool
nearAll pts crd =
  let distSum = sum . map (dist crd . pointCoord) $ pts
  in distSum < 10000


-- | counts up the number of coords nearest to a
-- point in a calculated grid and return the
-- point with the most associated coords
findLargestArea :: Grid -> Int
findLargestArea grd =
  let cnt = C.fromList [ (n, 1) | (_, n) <- Map.toList grd ]
  in snd $ C.maximum cnt


-- | calculated a @Grid for the given points
-- will remove any points from areas that touches
-- the boundary
grid :: Boundary -> [Point] -> Grid
grid ((x0,y0), (x1,y1)) pts =
  removeInfinitePoints $ makeGrid gridCoords
  where
    makeGrid = Map.fromList . mapMaybe findNearest
    removeInfinitePoints grd =
      let borderPoints = Set.fromList . mapMaybe (`Map.lookup` grd) $ borderCoords
      in Map.filter (not . (`Set.member` borderPoints)) grd
    findNearest coord =
      (coord,) <$> nearest pts coord
    gridCoords =
      [ (x,y) | x <- [x0..x1], y <- [y0..y1] ]
    borderCoords =
      [ (x0,y) | y <- [y0..y1] ]
      ++ [ (x1,y) | y <- [y0..y1] ]
      ++ [ (x, y0) | x <- [x0..x1] ]
      ++ [ (x, y1) | x <- [x0..x1] ]


-- | finds the nearest point to the given coordinate
-- if this point's distance is the unique minimum
nearest :: [Point] -> Coord -> Maybe PointNr
nearest pts coord =
  let dists = sortOn snd $ [ (n, dist coord p) | (Point n p) <- pts ]
  in case take 2 dists of
    (na, da) : (_, db) : _ | da < db -> Just na
    [(na, _)]                        -> Just na
    _                                -> Nothing


-- | Manhatten-Distance
dist :: Coord -> Coord -> Int
dist (x,y) (x',y') = abs (x'-x) + abs (y'-y)


-- | finds a simple enclosing rectangle
findBounds :: [Point] -> Boundary
findBounds input =
  ((minimum xs, minimum ys), (maximum xs, maximum ys))
  where xs = map (fst . pointCoord) input
        ys = map (snd . pointCoord) input


----------------------------------------------------------------------
-- IO

inputTxt :: IO [Point]
inputTxt = points . parseInput <$> readFile "./src/Day6/input.txt"


points :: [Coord] -> [Point]
points inp =
  [ Point n c | (n,c) <- zip [1..] inp ]


parseInput :: String -> [Coord]
parseInput = map parseCoordLine . lines


parseCoordLine :: String -> Coord
parseCoordLine = either (error . show) id . parse coordP "input.txt"


type Parser a = Parsec String () a

coordP :: Parser Coord
coordP = (,) <$> intP <* (char ',' >> spaces) <*> intP
  where
    intP :: Parser Int
    intP = read <$> many1 (satisfy isDigit)
