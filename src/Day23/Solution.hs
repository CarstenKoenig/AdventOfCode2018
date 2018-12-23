module Day23.Solution where

import Data.Char (isDigit)
import Data.List (maximumBy)
import Data.Ord (comparing)
import Text.Parsec


-- | (X,Y,Z)
type Coord = (Int, Int, Int)

type SendRadius = Int

type Distance = Int

data Nanobot = Nanobot
  { nanobotPos    :: !Coord
  , nanobotRadius :: !SendRadius
  } deriving (Show)


type Input = [Nanobot]


dist :: Coord -> Coord -> Distance
dist (x,y,z) (x',y',z') = abs (x'-x) + abs (y'-y) + abs (z'-z)


run :: IO ()
run = do
  putStrLn "DAY 23"
  inp <- inputTxt

  putStrLn $ "part 1: " ++ show (part1 inp)


part1 :: Input -> Int
part1 bots = length $ inRangeOf bots str
  where str = strongest bots


strongest :: [Nanobot] -> Nanobot
strongest = maximumBy (comparing nanobotRadius)


inRangeOf :: [Nanobot] -> Nanobot -> [Nanobot]
inRangeOf bots bot = filter inRange bots
  where
    inRange bot' = dist (nanobotPos bot) (nanobotPos bot') <= nanobotRadius bot


inputTxt :: IO Input
inputTxt = map parseLine . lines <$> readFile "./src/Day23/input.txt"


exampleTxt :: IO Input
exampleTxt = map parseLine . lines <$> readFile "./src/Day23/example.txt"


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
