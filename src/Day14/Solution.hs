module Day14.Solution where

import           Data.Char (isDigit)
import           Text.Parsec


run :: IO ()
run = do
  putStrLn "DAY 14"


inputTxt :: IO String
inputTxt = readFile "./src/Day14/input.txt"


type Input = ()

parseInput :: String -> Input
parseInput = either (error . show) id . parse inputP "input.txt"


type Parser a = Parsec String () a


inputP :: Parser Input
inputP = pure ()


intP :: Parser Int
intP = choice [ negate <$> (char '-' *> numP), numP ]


numP :: Parser Int
numP = read <$> many1 (satisfy isDigit)
