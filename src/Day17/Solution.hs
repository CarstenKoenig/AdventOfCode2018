module Day17.Solution where

import           Data.Char (isDigit)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Set (Set)
import qualified Data.Set as S
import           Text.Parsec


run :: IO ()
run = do
  putStrLn "DAY 17"


inputTxt :: IO ClayCoords
inputTxt = parseInput <$> readFile "./src/Day17/input.txt"


parseInput :: String -> ClayCoords
parseInput = either (error . show) id . parse inputP "input.txt"

parse' :: Parser a -> String -> a
parse' p = either (error . show) id . parse p "input.txt"

type Parser a = Parsec String () a


inputP :: Parser ClayCoords
inputP = S.fromList . concat <$> (many $ (coordP <|> coordP') <* many newline)


coordP :: Parser [Coord]
coordP = do
  xs <- coordCompP 'x'
  _ <- string ", "
  ys <- coordCompP 'y'
  pure $ [ (x,y) | x <- xs, y <- ys ]


coordP' :: Parser [Coord]
coordP' = do
  ys <- coordCompP 'y'
  _ <- string ", "
  xs <- coordCompP 'x'
  pure $ [ (x,y) | x <- xs, y <- ys ]


coordCompP :: Char -> Parser [Int]
coordCompP c = char c *> char '=' *> rangeP


rangeP :: Parser [Int]
rangeP = do
  start <- numP
  maybe [start] (\end -> [start..end]) <$> (optionMaybe $ do
    _ <- string ".."
    numP)


intP :: Parser Int
intP = choice [ negate <$> (char '-' *> numP), numP ]


numP :: Parser Int
numP = read <$> many1 (satisfy isDigit)


type Coord = (Int, Int)

type World = Map Coord Content

data Content
  = Sand
  | Clay
  | DrySand
  | Water
  deriving Show


type ClayCoords = Set Coord
