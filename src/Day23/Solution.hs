module Day23.Solution
  ( run
  ) where

import Control.Arrow ((&&&))
import Data.Char (isDigit)
import Data.List (maximumBy, minimumBy)
import Data.Ord (comparing)
import Text.Parsec


type Coord = (Int, Int, Int)

type SendRadius = Int

type Distance = Int

data Nanobot = Nanobot
  { nanobotPos    :: !Coord
  , nanobotRadius :: !SendRadius
  } deriving (Show)


type Input = [Nanobot]


----------------------------------------------------------------------
-- main

run :: IO ()
run = do
  putStrLn "DAY 23"
  inp <- inputTxt

  putStrLn $ "part 1: " ++ show (part1 inp)
  putStrLn $ "part 2: " ++ show (part2 inp)

----------------------------------------------------------------------
-- part 1

part1 :: Input -> Int
part1 bots = length $ inRangeOf bots str
  where str = strongest bots


strongest :: [Nanobot] -> Nanobot
strongest = maximumBy (comparing nanobotRadius)


inRangeOf :: [Nanobot] -> Nanobot -> [Nanobot]
inRangeOf bots bot = filter (coordInRange . nanobotPos) bots
  where
    coordInRange c = dist (nanobotPos bot) c <= nanobotRadius bot


dist :: Coord -> Coord -> Distance
dist (x,y,z) (x',y',z') = abs (x'-x) + abs (y'-y) + abs (z'-z)

----------------------------------------------------------------------
-- part 2

part2 :: Input -> Distance
part2 bots =
  let best = search bots
  in dist (0,0,0) best


-- | searches for the coord with the most bots in range
--  looks for coords in grid which distance is halfed in each step
--  chooses the best coord in such a grid by counting the bots in reach with any
--  point in a cube-cell in that grid (see 'intersect' bellow)
search :: [Nanobot] -> Coord
search bots = go startCubeRadius $ bounds bots
  where
    go 1 bds = bestCoord 1 $ candidates 1 bds
    go cubeRadius bds =
      if cubeRadius <= 1
      then bestC
      else go (cubeRadius `div` 2) ((x-cubeRadius, y-cubeRadius, z-cubeRadius), (x+cubeRadius,y+cubeRadius,z+cubeRadius))
      where
        bestC@(x,y,z) = bestCoord cubeRadius $ candidates cubeRadius bds


    -- find a power of 2 such that every bot is in the cube
    startCubeRadius =
      let ((minX, minY, minZ), (maxX, maxY, maxZ)) = bounds bots
      in findNextPower $ maximum [maxX - minX, maxY - minY, maxZ - minZ]

    -- find the coord in 'crds' where a cube around it with radius 'cubeRadius' intersects the most bots
    bestCoord cubeRadius crds =
      let
        coordAndCount = map (botsInRange cubeRadius &&& id) crds
        bestCount = maximum $ map fst coordAndCount
      in minimumBy (comparing $ dist (0,0,0)) $ map snd $ filter ((== bestCount) . fst) coordAndCount

    botsInRange cubeRadius c =
      length $ filter (intersect cubeRadius c) bots

    intersect cubeRadius c bot =
      dist c (nanobotPos bot) < cubeRadius + nanobotRadius bot

    -- candidates for coords are the vertices of all dividing sub-cubes
    candidates cubeLen bds =
      let ((minX, minY, minZ), (maxX, maxY, maxZ)) = bds
      in [(x,y,z) | x <- [minX,minX+cubeLen..maxX-1]
                  , y <- [minY,minY+cubeLen..maxY-1]
                  , z <- [minZ,minZ+cubeLen..maxZ-1]
                  ]

    findNextPower n = head $ dropWhile (< n) $ iterate (*2) 1


bounds :: [Nanobot] -> (Coord, Coord)
bounds bots = ((minX, minY, minZ), (maxX, maxY, maxZ))
  where
    minX = minimum $ map (\ (x,_,_) -> x) coords
    minY = minimum $ map (\ (_,y,_) -> y) coords
    minZ = minimum $ map (\ (_,_,z) -> z) coords
    maxX = maximum $ map (\ (x,_,_) -> x) coords
    maxY = maximum $ map (\ (_,y,_) -> y) coords
    maxZ = maximum $ map (\ (_,_,z) -> z) coords
    coords = map nanobotPos bots


----------------------------------------------------------------------
-- IO

inputTxt :: IO Input
inputTxt = map parseLine . lines <$> readFile "./src/Day23/input.txt"


----------------------------------------------------------------------
-- parsing

parseLine :: String -> Nanobot
parseLine = either (error . show) id . parse nanobotP "input.txt"


type Parser a = Parsec String () a


nanobotP :: Parser Nanobot
nanobotP = Nanobot <$> posP <* string ", " <*> radiusP


radiusP :: Parser SendRadius
radiusP = do
  _ <- string "r="
  r <- numP
  return r


posP :: Parser Coord
posP = do
  _ <- string "pos=<"
  x <- intP <* char ','
  y <- intP <* char ','
  z <- intP <* char '>'
  return (x,y,z)


intP :: Parser Int
intP = choice [ negate <$> (char '-' *> numP), numP ]


numP :: Parser Int
numP = read <$> many1 (satisfy isDigit)
