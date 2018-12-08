module Day8.Solution where

import           Data.Char (isDigit)
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import           Data.Monoid (Sum(..))
import           Text.Parsec


run :: IO ()
run = do
  putStrLn "DAY 8"

  node <- inputTxt
  putStrLn $ "part 1: " ++ show (getSum $ checkSum node)
  putStrLn $ "part 2: " ++ show (getSum $ value node)


-- | the input is a list of numbers
type Number = Int


-- | the problem represents a tree of nodes
-- each node has children and meta-data (numbers)
data Node = Node
  { children   :: IntMap Node
  , metadata   :: [Number]
  } deriving Show


-- | part 1:
-- the checksum is just the sum of metadata + the sum of checksum of all children
checkSum :: Node -> Sum Int
checkSum (Node chs meta) =
  foldMap Sum meta <> foldMap checkSum chs


-- | part 2:
-- if a node has no Children the value is just the sum of the metadata
-- if a node has Children the metadata represent indizes and the value
-- of the node becomes the sum of values of existing children
value :: Node -> Sum Int
value (Node chds meta) | null chds =
  foldMap Sum meta
value (Node chds ids) =
  foldMap getChildValue ids
  where
    getChildValue ind =
      maybe 0 value $ IntMap.lookup ind chds


----------------------------------------------------------------------
-- Parsing

type Parser a = Parsec String () a


-- | parses a stream of numbers into nodes
nodeP :: Parser Node
nodeP = do
  nrCh <- numberP
  nrMeta <- numberP
  child <- IntMap.fromList . zip [1..] <$> count nrCh nodeP
  meta <- count nrMeta numberP
  return $ Node child meta


-- | parses a number suffixed by spaces
numberP :: Parser Number
numberP = read <$> many1 (satisfy isDigit) <* spaces


----------------------------------------------------------------------
-- IO

parseInput :: String -> Node
parseInput = either (error . show) id . parse nodeP "input.txt"


inputTxt :: IO Node
inputTxt = parseInput <$> readFile "./src/Day8/input.txt"


----------------------------------------------------------------------
-- example data

example :: String
example = "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"


inputExample :: Node
inputExample = parseInput example
