module Day10.Solution
  ( run
  ) where

import Data.Char (isDigit)
import Data.List (sortBy, groupBy, nub)
import Data.Ord (comparing)
import Text.Parsec
import Control.Monad (mapM_)
import Data.Tuple (swap)
import Data.Function (on)


type Input = [Point]

data Point = Point
  { pos :: Pos
  , speed :: Speed
  } deriving Show

type Pos = (Int, Int)
type Speed = (Int, Int)

----------------------------------------------------------------------
-- main

run :: IO ()
run = do
  putStrLn "DAY 10"
  inp <- inputTxt

  putStrLn "part 1:"
  let (steps, minInp) = findMin inp
  showText minInp

  putStrLn $ "part 2:" ++ show steps


----------------------------------------------------------------------
-- text output

-- | outputs the input as text (see the problem)
showText :: Input -> IO ()
showText = mapM_ putStrLn . textLines


textLines :: Input -> [String]
textLines inp = map formatLine lineXs
  where
    lineXs              = map transformCoordsToXs . groupLines $ sortedByYandX
    minX                = minimum . map (fst . pos) $ inp
    sortedByYandX       = sortBy (comparing swap) . map pos $ inp
    groupLines          = groupBy ((==) `on` snd)
    transformCoordsToXs = nub . map (subtract minX . fst)
    formatLine          = go 0
      where
        go lastX (x:xs) = replicate (x-lastX) ' ' ++ '#' : go (x+1) xs
        go _ []         = ""


----------------------------------------------------------------------
-- find the text

-- | idea is simple: iterate till the "line-height" stops decreasing the first time
-- the line-height is the difference between the largest and smallest y-coordinate of any point
findMin :: Input -> (Int, Input)
findMin input = go 0 (maxYDiff input) input
  where
    go steps lstDiff inp =
      let inp' = step inp
          diff = maxYDiff inp'
      in if diff > lstDiff then (steps, inp) else go (steps+1) diff inp'


-- | calculates the difference between the largest and smallest y-coord
maxYDiff :: Input -> Int
maxYDiff pts = maxY - minY
  where minY = minimum . map (snd . pos) $ pts
        maxY = maximum . map (snd . pos) $ pts


-- | moves ever point in the input once
step :: Input -> Input
step = map stepPoint


-- | moves the point
stepPoint :: Point -> Point
stepPoint (Point (px,py) v@(vx,vy)) =
  Point (px+vx,py+vy) v


----------------------------------------------------------------------
-- IO input

inputTxt :: IO Input
inputTxt = map parseLine . lines <$> readFile "./src/Day10/input.txt"

parseLine :: String -> Point
parseLine = either (error . show) id . parse pointP "input.txt"

type Parser a = Parsec String () a

pointP :: Parser Point
pointP = do
  _ <- string "position="
  p <- pairParser
  _ <- string "velocity="
  v <- pairParser
  return $ Point p v

pairParser :: Parser (Int, Int)
pairParser = do
  _ <- char '<' <* spaces
  x <- intP
  _ <- char ',' <* spaces
  y <- intP
  _ <- char '>' <* spaces
  return (x,y)

intP :: Parser Int
intP = choice [ negate <$> (char '-' *> numP), numP ]

numP :: Parser Int
numP = read <$> many1 (satisfy isDigit)
