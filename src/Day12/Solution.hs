module Day12.Solution where

import           Data.Char (isDigit)
import           Text.Parsec


run :: IO ()
run = do
  putStrLn "DAY 12"


inputTxt :: IO String
inputTxt = readFile "./src/Day12/input.txt"


type Input = ()

parseInput :: String -> Input
parseInput = either (error . show) id . parse inputP "input.txt"


type Parser a = Parsec String () a


inputP :: Parser Input
inputP = pure ()
  where
    intP :: Parser Int
    intP = read <$> many1 (satisfy isDigit)