-- | sollution of Day 11 using a [summed-area table](https://en.wikipedia.org/wiki/Summed-area_table) approach

module Day11.Solution (run) where

import           Data.Array (Array, (!))
import qualified Data.Array as A
import           Data.Ix (range)
import           Data.List (maximumBy)
import           Data.Ord (comparing)

-- | for each coord (x,y) this table holds the sum of
-- the arrea in a grid starting at (1,1) to (x,y)
type Grid = Array Coord PowerLevel

type Coord = (Int, Int)
type PowerLevel = Int
type Size = Int
type GridSerial = Int


run :: IO ()
run = do
  putStrLn "DAY 11"
  let grid = powerGrid gridSerial
  putStrLn $ "part 1: " ++ show (findMax 3 grid)
  putStrLn $ "part 2: " ++ show (findMax 300 grid)


-- | this is my problem number for today - yours might differ
gridSerial :: GridSerial
gridSerial = 4842


-- | enumerate all valid areas in the grid and find the one
-- with maximum power-level sum using the summed-area table
findMax :: Size -> Grid -> (Coord, Size)
findMax maxSize grid =
  maximumBy (comparing (\(c@(x,y), sz) -> areaPowerLevel grid (c,(x+sz,y+sz)))) $
  [ (c, sz) | c@(x,y) <- A.indices grid, sz <- [1..min maxSize (301 - max x y) ] ]


-- | calcualtes the summed power-level using a summed-area-table
-- see [Wikipedia](https://en.wikipedia.org/wiki/Summed-area_table#The_algorithm)
-- ![Sum = D - B - C + A](https://upload.wikimedia.org/wikipedia/commons/thumb/5/58/Summed_area_table.png/220px-Summed_area_table.png)
areaPowerLevel :: Grid -> (Coord, Coord) -> PowerLevel
areaPowerLevel grid (c@(x,y),c'@(x',y'))
  | A.inRange gridBounds c && A.inRange gridBounds c' = grid!c' + grid!c - grid!(x',y) - grid!(x,y')
  | otherwise = 0


-- | calculates a summed-area-table from the cached power-level values
powerGrid :: GridSerial -> Grid
powerGrid gS =
  let arr = A.array gridBounds [(c, sumUL c) | c <- gridCoords ]
      sumUL (1,1)   = powerLevel (1,1)
      sumUL (!x,1)  = powerLevel (x,1) + sumUL (x-1,1)
      sumUL (1,!y)  = powerLevel (1,y) + sumUL (1,y-1)
      sumUL (!x,!y) = powerLevel (x,y) + arr ! (x,y-1) + arr ! (x-1,y) - arr ! (x-1,y-1)
  in arr
  where
    powerLevel (x,y) =
      hundredsDigit ((rackId * y + gS) * rackId) - 5
      where rackId   = x + 10
    hundredsDigit n  = (n `mod` 1000) `div` 100


gridBounds :: (Coord, Coord)
gridBounds = ((1,1), (300,300))


gridCoords :: [Coord]
gridCoords = range gridBounds
