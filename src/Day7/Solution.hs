module Day7.Solution where

import           Data.Char (isDigit)
import qualified Data.Graph as G
import           Text.Parsec
import Data.List (sort, groupBy, sortBy, foldl')
import Data.Ord (comparing)
import qualified Utils.Counter as C
import Data.Maybe (listToMaybe)



run :: IO ()
run = do
  putStrLn "DAY 7"


part1 :: Input -> String
part1 = topoSort


topoSort :: Input -> [Char]
topoSort inp = go $ degrees inp
  where
    go :: C.Counter Char Int -> [Char]
    go degs = do
      c <- maybe [] pure $ degree0 degs
      let degs' = updateIncoming c $ C.remove c degs
      c : go degs'
    updateIncoming :: Char -> C.Counter Char Int -> C.Counter Char Int
    updateIncoming from cnt =
      foldl' (\ cnt' f -> C.decr f cnt') cnt $ map snd $ filter (\(f',_) -> f' == from) inp


degrees :: Input -> C.Counter Char Int
degrees inp = C.fromList $ concat [ [(f, 0), (t,1)] | (f,t) <- inp]


degree0 :: C.Counter Char Int -> Maybe Char
degree0 = listToMaybe . map fst . sortBy (comparing fst) . filter (\ (_,n) -> n == 0) . C.asc


graph :: Input -> (G.Graph, G.Vertex -> Char)
graph inp =
  let (g, fromVertex, _) = G.graphFromEdges $ edges inp
  in (g, \v -> let (n,_,_) = fromVertex v in n)


edges :: Input -> [(Char, Char, [Char])]
edges inp = 
  let grps = groupBy (\(a,_) (b,_) -> a == b) $ sort inp
  in [ (from, from, reverse $ sort $ map snd tos) | tos  <- grps, let from = fst $ head tos ]


inputTxt :: IO Input
inputTxt = map parseLine . lines <$> readFile "./src/Day7/input.txt"

inputTst :: IO Input
inputTst = map parseLine . lines <$> readFile "./src/Day7/test.txt"

type Input = [Order]


parseLine :: String -> Order
parseLine = either (error . show) id . parse orderP "input.txt"


type Parser a = Parsec String () a


orderP :: Parser Order
orderP = (,) <$> (string "Step " *> anyChar <* string " must be finished before step ") <*> (anyChar <* string " can begin.")


type Order
  = (Char, Char)

