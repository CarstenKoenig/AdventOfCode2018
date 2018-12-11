module Day11.Solution (run) where

import           Data.Array (Array, (!))
import qualified Data.Array as A
import           Data.List (maximumBy, scanl')
import           Data.Ord (comparing)


type Grid = Array Coord PowerLevel

type Coord = (Int, Int)
type PowerLevel = Int


findMaxSquareCoord :: Int -> (Coord, Int)
findMaxSquareCoord gS =
  (\(c,sz,_) -> (c,sz)) $ maximumBy (comparing (\(_,_,pl) -> pl)) . concat $ [ partialTotalPower grid (c, 301 - max x y) | c@(x,y) <- coords ]
  where grid = powerGrid gS



partialTotalPower :: Grid -> (Coord, Int) -> [(Coord, Int, PowerLevel)]
partialTotalPower grid (c,sz) =
  tail $ scanl' addPartial (c, 0, 0) (partialSquareCoords sz c)
  where
    addPartial :: (Coord, Int, PowerLevel) -> ([Coord], Int) -> (Coord, Int, PowerLevel)
    addPartial (_, _, total) (cs, s) = (c, s, total + sum ((grid !) <$> cs))


partialSquareCoords :: Int -> Coord -> [([Coord], Int)]
partialSquareCoords sz (cx,cy) = [ (border i, i) | i <- [1..sz] ]
  where
    border 1 = [(cx,cy)]
    border n = [ (cx+n-1,y) | y <- [cy..cy+n-2] ] ++ (cx+n-1,cy+n-1) :  [(x,cy+n-1) | x <- [cx+n-2,cx+n-3..cx] ]


gridBounds :: (Coord, Coord)
gridBounds = ((1,1), (300,300))


coords :: [Coord]
coords = [ (x,y) | y <- [1..300], x <- [1..300]]


powerGrid :: Int -> Grid
powerGrid gS = A.array gridBounds [ (c, powerLevel gS c) | c <- coords ]


powerLevel :: Int -> Coord -> PowerLevel
powerLevel gS (x,y) =
  hundredsDigit ((rackId * y + gS) * rackId) - 5
  where
    hundredsDigit n = (n `mod` 1000) `div` 100
    rackId          = x + 10


run :: IO ()
run = do
  putStrLn "DAY 11"

  putStrLn $ "part 2: " ++ show (findMaxSquareCoord gridSerial)


gridSerial :: Int
gridSerial = 4842
