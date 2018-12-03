module Day3.Solution where

import           Data.Char (isDigit)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Text.Parsec


type Coord = (Int, Int)


-- | the Fabric - a map of coordinates with counters how often the spot was marked
type Fabric = Map Coord Int


-- | a claim as given by the problem
data Claim = Claim
  { claimId :: Int
  , left :: Int
  , top :: Int
  , width :: Int
  , height :: Int
  } deriving (Show)


----------------------------------------------------------------------
-- run / parts 1/2

run :: IO ()
run = do
  putStrLn "DAY 3"
  claims <- inputTxt
  let fab = fabric claims
  putStrLn $ "part 1: " ++ show (part1 fab)
  putStrLn $ "part 2: " ++ show (part2 fab claims)


part1 :: Fabric -> Int
part1 = overlappArea


part2 :: Fabric -> [Claim] -> Int
part2 fab claims =
  claimId . head $ filter (notOverlapping fab) claims


-----------------------------------------------------------------------
-- problem domain


-- | initializes a Fabric marking a list of claims
fabric :: [Claim] -> Fabric
fabric = foldr mark Map.empty


-- | checks if a claim was not overlapped
-- by validating that all points of the claim were only marked once
notOverlapping :: Fabric -> Claim -> Bool
notOverlapping fab claim =
  all onlyOnceInFabric $ points claim
  where
    onlyOnceInFabric pt = Map.lookup pt fab == Just 1


-- | the area of all the overlapps is just the number of points with
-- more than one mark
overlappArea :: Fabric -> Int
overlappArea =
  length . Map.toList . Map.filter (> 1)


-- | increases the mark of all points in the fabric that lie in the claims region
mark :: Claim -> Fabric -> Fabric
mark claim =
  flip (foldr markPoint) (points claim)


-- | a list of all the coordinates/spots in the claim
points :: Claim -> [Coord]
points claim = [(left claim + x, top claim + y) | x <- [0..width claim - 1], y <- [0..height claim - 1] ]


-- | marks a Coord in the Fabric = increases the coordinats counter
markPoint :: Coord -> Fabric -> Fabric
markPoint c = Map.insertWith (+) c 1


----------------------------------------------------------------------
-- file input

inputTxt :: IO [Claim]
inputTxt = map parseLine . lines <$> readFile "./src/Day3/input.txt"


----------------------------------------------------------------------
-- Parsing

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
