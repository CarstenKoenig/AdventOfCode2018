module Day23.Solution
  ( run
  ) where

import           Data.Char (isDigit)
import           Data.List (maximumBy, minimumBy)
import           Data.Ord (comparing)
import           Text.Parsec


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


search :: [Nanobot] -> Coord
search bots = go startDist $ bounds bots
  where
    go 1 bds = bestCoord 1 $ candidates 1 bds
    go d bds =
      let (x,y,z) = bestCoord d $ candidates d bds
      in go (d `div` 2) ((x-d, y-d, z-d), (x+d,y+d,z+d))

    startDist =
      let ((minX, _, _), (maxX, _, _)) = bounds bots
      in findNextPower (maxX - minX)

    bestCoord d crds =
      let
        coordAndCount = map (\c -> (botsInRange d c, c)) crds
        bestCount = maximum $ map fst coordAndCount
      in minimumBy (comparing $ dist (0,0,0)) $ map snd $ filter ((== bestCount) . fst) coordAndCount

    botsInRange d c =
      length $ filter (intersect d c) bots

    intersect d c bot =
      dist c (nanobotPos bot) < d + nanobotRadius bot

    candidates d bds =
      let ((minX, minY, minZ), (maxX, maxY, maxZ)) = bds
      in [(x,y,z) | x <- [minX,minX+d..maxX]
                  , y <- [minY,minY+d..maxY]
                  , z <- [minZ,minZ+d..maxZ]
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
