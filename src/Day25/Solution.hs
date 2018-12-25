module Day25.Solution where

import           Data.Char (isDigit)
import           Data.List (foldl')
import           Text.Parsec
import           Utils.Graph.ConnectedGroups


type Input = [Coord]

type Coord = (Int, Int, Int, Int)

----------------------------------------------------------------------
-- main

run :: IO ()
run = do
  putStrLn "DAY 25"
  inp <- inputTxt
  putStrLn $ "part 1: " ++ show (part1 inp)


-- | count all connected groups of a graph
-- created by the problem input
part1 :: Input -> Int
part1 = length . groups . mkGraph


-- | turns the problems input into a graph
-- just connects every coord with those that
-- are at max 3 dist away (it's a undirected graph so one direction is ok)
mkGraph :: Input -> Graph
mkGraph inp = foldl' addConns emptyGraph $ coordPairs $ addNrs inp
  where
    addConns gr ((nra, a), (nrb, b))
      | dist a b <= 3 = insertCon (nra, nrb) gr
      | otherwise     = gr

    coordPairs [] = []
    coordPairs (x:xs) = (x,x) : [ (x,x') | x' <- xs ] ++ coordPairs xs

    addNrs = zip [0..]


-- | manhatten dist
dist :: Coord -> Coord -> Int
dist (x,y,z,t) (x',y',z',t') =
  abs (x'-x) + abs (y'-y) + abs (z'-z) + abs (t'-t)

----------------------------------------------------------------------
-- IO

inputTxt :: IO Input
inputTxt = inputFile "input.txt"


inputFile :: FilePath -> IO Input
inputFile fileName = map parseLine . lines <$> readFile ("./src/Day25/" ++ fileName)


parseLine :: String -> Coord
parseLine = either (error . show) id . parse coordP "input.txt"


----------------------------------------------------------------------
-- parsing

type Parser a = Parsec String () a


coordP :: Parser Coord
coordP = (,,,) <$> (intP <* char ',') <*> (intP <* char ',') <*> (intP <* char ',') <*> intP


intP :: Parser Int
intP = choice [ negate <$> (char '-' *> numP), numP ]


numP :: Parser Int
numP = read <$> many1 (satisfy isDigit)
