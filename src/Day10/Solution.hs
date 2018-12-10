module Day10.Solution where

import Data.Char (isDigit)
import Data.List (sortBy, sort, groupBy, nub)
import Data.Ord (comparing)
import Text.Parsec
import Control.Monad (mapM_)
import Data.Tuple (swap)


run :: IO ()
run = do
  putStrLn "DAY 10"
  inp <- inputTxt

  putStrLn "part 1:"
  let (steps, minInp) = findMin inp
  showText minInp

  putStrLn $ "part 2:" ++ show steps


part1 :: Input -> Input
part1 = head . posLines


showText :: Input -> IO ()
showText = mapM_ putStrLn . textLines


textLines :: Input -> [String]
textLines inp =
  let lineXs = map (nub . map (subtract minX . fst)) . groupBy (\(_,y) (_,y') -> y == y') . sortBy (comparing swap) . map pos $ inp
  in map printLine lineXs
  where
    minX = minimum . map (fst . pos) $ inp
    maxX = subtract minX $ maximum . map (fst . pos) $ inp
    printLine = go 0
      where
        go :: Int -> [Int] -> String
        go p [] = replicate (maxX - p + 1) '.'
        go p (d:ds) = replicate (d - p) '.' ++ '#' : go (d+1) ds


posLines :: Input -> [Input]
posLines = filter inLine . animation


inLine :: Input -> Bool
inLine = (== 7) . maxYDiff


findMin :: Input -> (Int, Input)
findMin input = go 0 (maxYDiff input) input
  where
    go steps lstDiff inp =
      let inp' = step inp
          diff = maxYDiff inp'
      in if diff > lstDiff then (steps, inp) else go (steps+1) diff inp'


maxYDiff :: Input -> Int
maxYDiff pts = maxY - minY
  where minY = minimum . map (snd . pos) $ pts
        maxY = maximum . map (snd . pos) $ pts


animation :: Input -> [Input]
animation = iterate step


step :: Input -> Input
step = map stepPoint


stepPoint :: Point -> Point
stepPoint (Point (px,py) v@(vx,vy)) =
  Point (px+vx,py+vy) v


inputTxt :: IO Input
inputTxt = map parseLine . lines <$> readFile "./src/Day10/input.txt"


inputTst :: IO Input
inputTst = map parseLine . lines <$> readFile "./src/Day10/test.txt"


type Input = [Point]

parseLine :: String -> Point
parseLine = either (error . show) id . parse pointP "input.txt"


type Parser a = Parsec String () a


data Point = Point
  { pos :: Pos
  , speed :: Speed
  } deriving Show


pointP :: Parser Point
pointP = do
  _ <- string "position="
  p <- pairParser
  _ <- string "velocity="
  v <- pairParser
  return $ Point p v


type Pos = (Int, Int)
type Speed = (Int, Int)

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
