module Day8.Solution where

import           Data.Char (isDigit)
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import           Text.Parsec


run :: IO ()
run = do
  putStrLn "DAY 8"

  node <- inputTxt
  putStrLn $ "part 1: " ++ show (checkSum node)
  putStrLn $ "part 2: " ++ show (value node)


type Input = [Number]
type Number = Int


data Node = Node
  { nrChildren :: Int
  , nrMetadata :: Int
  , children   :: IntMap Node
  , metadata   :: [Number]
  } deriving Show


checkSum :: Node -> Int
checkSum (Node _ _ chs meta) =
  sum meta + sum (map checkSum $ IntMap.elems chs)


value :: Node -> Int
value (Node 0 _ _ meta) = sum meta
value (Node _ _ chds ids) =
  sum $ map getChildValue ids
  where
    getChildValue ind =
      maybe 0 value $ IntMap.lookup ind chds

parseInput :: String -> Node
parseInput = either (error . show) id . parse nodeP "input.txt"


type Parser a = Parsec String () a


nodeP :: Parser Node
nodeP = do
  nrCh <- numberP
  nrMeta <- numberP
  child <- IntMap.fromList . zip [1..] <$> count nrCh nodeP
  meta <- count nrMeta numberP
  return $ Node nrCh nrMeta child meta


numberP :: Parser Number
numberP = read <$> many1 (satisfy isDigit) <* spaces


example :: String
example = "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"


inputExample :: Node
inputExample = parseInput example


inputTxt :: IO Node
inputTxt = parseInput <$> readFile "./src/Day8/input.txt"

