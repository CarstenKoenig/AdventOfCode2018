module Day3.Solution where

import           Data.Char (isDigit)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Text.Parsec


run :: IO ()
run = do
  putStrLn "DAY 3"
  claims <- inputTxt
  let fab = fabric claims
  putStrLn $ "part 1: " ++ show (part1 fab)
  putStrLn $ "part 2: " ++ show (part2 fab claims)


inputTxt :: IO [Claim]
inputTxt = map parseLine . lines <$> readFile "./src/Day3/input.txt"


part1 :: Fabric -> Int
part1 = overlappArea


part2 :: Fabric -> [Claim] -> Int
part2 fab claims =
  claimId . head $ filter (notOverlapping fab) claims

type Coord = (Int, Int)

type Fabric = Map Coord Int


fabric :: [Claim] -> Fabric
fabric = foldr mark Map.empty


notOverlapping :: Fabric -> Claim -> Bool
notOverlapping fab claim =
  all onceInClaim $ points claim
  where
    onceInClaim pt = Map.lookup pt fab == Just 1


overlappArea :: Fabric -> Int
overlappArea = length . Map.toList . Map.filter (> 1)


mark :: Claim -> Fabric -> Fabric
mark claim = flip (foldr markPoint) (points claim)

points :: Claim -> [Coord]
points claim = [(left claim + x, top claim + y) | x <- [0..width claim - 1], y <- [0..height claim - 1] ]


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
