module Day6.Solution where

import           Data.Char (isDigit)
import           Data.List (nub, sortOn)
import           Data.Maybe (fromJust, isJust)
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
-- lists all coordinates that have a unique
-- nearest point with this point, if it does not
-- lie in an infinite area
type Grid = [(Coord, PointNr)]


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
part2 = coordsNearAll


----------------------------------------------------------------------
-- Algorithm


-- | just count all Coordinates that are @nearAll points
coordsNearAll :: Boundary -> [Point] -> Int
coordsNearAll ((x0,y0), (x1,y1)) crds =
  length . filter (nearAll crds) $ [ (x,y) | x <- [x0-50..x1+50], y <- [y0-50..y1+50]]


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
  let cnt = C.fromList [ (n, 1) | (_, n) <- grd ]
  in snd $ C.maximum cnt


-- | calculated a @Grid for the given points
-- for each coord in the boundary the nearest
-- point is located - will be excluded if the point is
-- the nearest to a border-point
grid :: Boundary -> [Point] -> Grid
grid ((x0,y0), (x1,y1)) pts =
  let nearBorder = nearestBorder ((x0,y0), (x1,y1)) pts
  in [ (c, fromJust near) | x <- [x0..x1], y <- [y0..y1]
                          , let c = (x,y)
                          , let near = nearest pts c
                          , isJust near
                          , not (fromJust near `elem` nearBorder)
                          ]


-- | returns all points that are the nearest to a border
-- point - because those will have an infinite area
-- associated
nearestBorder :: Boundary -> [Point] -> [PointNr]
nearestBorder ((x0,y0), (x1,y1)) pts =
  nub [ nearest' pts c | c <- borderCoords ]
  where
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


-- | finds one nearest point to the given coord
-- will be the first one with minimal distance
nearest' :: [Point] -> Coord -> PointNr
nearest' pts coord =
  fst . head . sortOn snd $ [ (n, dist coord p) | (Point n p) <- pts ]


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
