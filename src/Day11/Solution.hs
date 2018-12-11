module Day11.Solution where

import           Data.Array (Array, (!))
import qualified Data.Array as A
import           Data.List (maximumBy)
import Data.Ord (comparing)


type Grid = Array Coord PowerLevel

type Coord = (Int, Int)
type PowerLevel = Int


findMaxSquareCoord :: Int -> (Coord, Int)
findMaxSquareCoord gS = maximumBy (comparing (totalPower grid)) $ [ c | i <- [1..300], c <- allSquareCoords i ]
  where grid = powerGrid gS



findMaxSquareCoord' :: Int -> Int -> (Coord, Int)
findMaxSquareCoord' gS sz = maximumBy (comparing (totalPower grid)) $ allSquareCoords sz
  where grid = powerGrid gS


allSquareCoords :: Int -> [(Coord,Int)]
allSquareCoords size = [ ((x,y), size) | x <- [1..301-size], y <- [1..301-size] ]


totalPower :: Grid -> (Coord, Int) -> PowerLevel
totalPower grid sqCoord = sum . map (grid !) $ squareCoords sqCoord


gridBounds :: (Coord, Coord)
gridBounds = ((1,1), (300,300))


coords :: [Coord]
coords = [ (x,y) | y <- [1..300], x <- [1..300]]


squareCoords :: (Coord,Int) -> [Coord]
squareCoords ((x,y), sz) =
  [ (x+dx, y+dy) | dx <- [0..sz-1], dy <- [0..sz-1] ]


powerGrid :: Int -> Grid
powerGrid gS = A.array gridBounds [ (c, powerLevel' gS c) | c <- coords ]


powerLevel :: Coord -> PowerLevel
powerLevel = powerLevel' gridSerial


powerLevel' :: Int -> Coord -> PowerLevel
powerLevel' gS (x,y) =
  hundredsDigit ((rackId * y + gS) * rackId) - 5
  where
    hundredsDigit n = (n `mod` 1000) `div` 100
    rackId          = x + 10


run :: IO ()
run = do
  putStrLn "DAY 11"

  putStrLn $ "part 1: " ++ show (findMaxSquareCoord gridSerial)

gridSerial :: Int
gridSerial = 4842
