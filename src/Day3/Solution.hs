module Day3.Solution where

import Text.Parsec
import Text.Parsec.Char
import Data.Char (isDigit)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

run :: IO ()
run = do
  putStrLn "DAY 3"
  inp <- inputTxt
  putStrLn $ "part 1: " ++ show (part1 inp)


inputTxt :: IO [Claim]
inputTxt = map parseLine . lines <$> readFile "./src/Day3/input.txt"


part1 :: [Claim] -> Int
part1 = overlappArea . foldr mark fabric

type Coord = (Int, Int)

type Fabric = Map Coord Int

fabric :: Fabric
fabric = Map.empty


overlappArea :: Fabric -> Int
overlappArea = length . Map.toList . Map.filter (> 1)


mark :: Claim -> Fabric -> Fabric
mark claim = flip (foldr markPoint) points
  where
    points = [(left claim + x, top claim + y) | x <- [0..width claim - 1], y <- [0..height claim - 1] ]


markPoint :: Coord -> Fabric -> Fabric
markPoint c = Map.insertWith (+) c 1


data Claim = Claim
  { claimId :: Int
  , left :: Int
  , top :: Int
  , width :: Int
  , height :: Int
  } deriving (Show)


parseLine :: String -> Claim
parseLine = either (error . show) id . parse lineP "input.txt"


type Parser a = Parsec String () a


lineP :: Parser Claim
lineP = Claim <$> idP <*> leftP <*> rightP <*> widthP <*> heightP
  where
    idP = char '#' *> intP <* whitespace
    leftP = char '@' *> whitespace *> intP
    rightP = char ',' *> intP <* char ':' <* whitespace
    widthP = intP <* char 'x'
    heightP = intP
    whitespace = many (char ' ')
    intP = read <$> many1 (satisfy isDigit)
