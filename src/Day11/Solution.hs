module Day11.Solution (run) where

import           Data.Array (Array, (!))
import qualified Data.Array as A
import           Data.Ix (range)
import           Data.List (maximumBy, scanl')
import           Data.Ord (comparing)


type Grid = Array Coord PowerLevel

type Coord = (Int, Int)
type PowerLevel = Int
type Size = Int
type GridSerial = Int


findMaxSquareCoord :: GridSerial -> Size -> (Coord, Size)
findMaxSquareCoord gS maxSz = (\(c,sz,_) -> (c,sz)) $
  maximumBy (comparing (\(_,_,pl) -> pl)) . concat $
  [ partialTotalPower grid (c, min maxSz (301 - max x y)) | c@(x,y) <- coords ]
  where
    grid = powerGrid gS
    coords = coordsInGrid grid


partialTotalPower :: Grid -> (Coord, Size) -> [(Coord, Size, PowerLevel)]
partialTotalPower grid (c,sz) =
  tail $ scanl' addPartial (c, 0, 0) (partialSquareCoords sz c)
  where
    addPartial (_, _, total) (cs, s) =
      (c, s, total + sum (powerLevel grid <$> cs))


partialSquareCoords :: Size -> Coord -> [([Coord], Size)]
partialSquareCoords sz (cx,cy) = [ (border i, i) | i <- [1..sz] ]
  where
    border 1 = [(cx,cy)]
    border n = [ (cx+n-1,y) | y <- [cy..cy+n-2] ] ++ (cx+n-1,cy+n-1) :  [(x,cy+n-1) | x <- [cx+n-2,cx+n-3..cx] ]


powerLevel :: Grid -> Coord -> PowerLevel
powerLevel = (!)


powerGrid :: GridSerial -> Grid
powerGrid gS = A.array gridBounds [ (c, getLevel c) | c <- coords ]
  where
    gridBounds = ((1,1), (300,300))
    coords     = range gridBounds
    getLevel (x,y) =
      hundredsDigit ((rackId * y + gS) * rackId) - 5
      where rackId   = x + 10
    hundredsDigit n  = (n `mod` 1000) `div` 100


coordsInGrid :: Grid -> [Coord]
coordsInGrid = A.indices


run :: IO ()
run = do
  putStrLn "DAY 11"

  putStrLn $ "part 1: " ++ show (findMaxSquareCoord gridSerial 3)
  putStrLn $ "part 2: " ++ show (findMaxSquareCoord gridSerial 10)


gridSerial :: GridSerial
gridSerial = 4842
