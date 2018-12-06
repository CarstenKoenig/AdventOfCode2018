module Day6.Solution where

import           Data.Char (isDigit)
import           Data.List (minimumBy, nub, sortOn)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromJust, isJust)
import           Data.Ord (comparing)
import           Text.Parsec
import           Utils.Counter (Counter)
import qualified Utils.Counter as C


type Input = [Coord]
type Coord = (Int,Int)


run :: IO ()
run = do
  putStrLn "DAY 6"
  inp <- inputTxt
  let bound = findBounds inp
  let pts = points inp
  let grd = grid bound pts
  let maxArea = findLargestArea grd
  putStrLn $ "part 1: " ++ show maxArea


part1 :: Input -> Int
part1 inp =
  let bound = findBounds inp
      pts = points inp
      grd = grid bound pts
  in  findLargestArea grd


findLargestArea :: Map Coord PointNr -> Int
findLargestArea grd =
  let cnt = C.fromList [ (n, 1) | (_, n) <- Map.toList grd ]
  in snd $ C.maximum cnt


grid :: (Coord, Coord) -> [Point] -> Map Coord PointNr
grid ((x0,y0), (x1,y1)) pts =
  let nearBorder = nearestBorder ((x0,y0), (x1,y1)) pts
  in Map.fromList [ (c, fromJust near) | x <- [x0..x1], y <- [y0..y1], let c = (x,y), let near = nearest pts c, isJust near, not (fromJust near `elem` nearBorder) ]


nearestBorder :: (Coord, Coord) -> [Point] -> [PointNr]
nearestBorder ((x0,y0), (x1,y1)) pts =
  nub [ nearest' pts c | c <- borderCoords ]
  where
    borderCoords =
      [ (x0,y) | y <- [y0..y1] ]
      ++ [ (x1,y) | y <- [y0..y1] ]
      ++ [ (x, y0) | x <- [x0..x1] ]
      ++ [ (x, y1) | x <- [x0..x1] ]


nearest :: [Point] -> Coord -> Maybe PointNr
nearest pts coord =
  let dists = sortOn snd $ [ (n, dist coord p) | (Point n p) <- pts ]
  in case take 2 dists of
    (na, da) : (nb, db) : _ | da < db -> Just na
    [(na, da)]                        -> Just na
    _                                 -> Nothing

nearest' :: [Point] -> Coord -> PointNr
nearest' pts coord =
  fst . head . sortOn snd $ [ (n, dist coord p) | (Point n p) <- pts ]


dist :: Coord -> Coord -> Int
dist (x,y) (x',y') = abs (x'-x) + abs (y'-y)


data Point = Point PointNr Coord
  deriving Show


type PointNr = Int


points :: Input -> [Point]
points inp =
  [ Point n c | (n,c) <- zip [1..] inp ]


findBounds :: Input -> (Coord, Coord)
findBounds input =
  ((minimum xs, minimum ys), (maximum xs, maximum ys))
  where xs = map fst input
        ys = map snd input


inputTxt :: IO Input
inputTxt = parseInput <$> readFile "./src/Day6/input.txt"


parseInput :: String -> Input
parseInput = map parseCoordLine . lines


parseCoordLine :: String -> Coord
parseCoordLine = either (error . show) id . parse coordP "input.txt"


type Parser a = Parsec String () a

coordP :: Parser Coord
coordP = (,) <$> intP <* (char ',' >> spaces) <*> intP
  where
    intP :: Parser Int
    intP = read <$> many1 (satisfy isDigit)

inputTest :: IO Input
inputTest = parseInput <$> readFile "./src/Day6/test.txt"
