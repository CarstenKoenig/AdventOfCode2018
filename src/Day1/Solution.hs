module Day1.Solution where

import           Data.IntSet (IntSet)
import qualified Data.IntSet as Set

run :: IO ()
run = do
  putStrLn "day1"
  myInput <- input
  putStrLn $ "star 1: " ++ show (solution1 myInput)
  putStrLn $ "star 2: " ++ show (solution2 myInput)


type Input = [Int]
type Output = Int


input :: IO Input
input = map parseNr . lines <$> readFile "./src/Day1/input.txt"
  where
    parseNr ('-':n) = negate $ read n
    parseNr ('+':n) = read n


solution1 :: Input -> Output
solution1 = sum

solution2 :: Input -> Output
solution2 numbers = addToDuplicate Set.empty $ states $ cycle numbers
  where
    states = scanl (+) 0
    addToDuplicate seen (n:ns)
      | isDuplicate seen n = n
      | otherwise          = addToDuplicate (Set.insert n seen) ns
    isDuplicate seen n =
      Set.member n seen
