module Day1.Solution where

import qualified Data.IntSet as Set


run :: IO ()
run = do
  putStrLn "DAY 1"
  myInput <- input
  putStrLn $ "star 1: " ++ show (solution1 myInput)
  putStrLn $ "star 2: " ++ show (solution2 myInput)


type Input = [Int]
type Output = Int


-- | input is either `+[int]` or `-[int]`
input :: IO Input
input = map parseNr . lines <$> readFile "./src/Day1/input.txt"
  where
    parseNr ('-':n) = negate $ read n
    parseNr ('+':n) = read n
    parseNr _       = error "invalid input"



-- | just sum up the input
solution1 :: Input -> Output
solution1 = sum


-- | repeat the input sequence, adding it to generate iterative
--   states and then search for the first such sum seen twice
solution2 :: Input -> Output
solution2 = insertTillDuplicate Set.empty . states . cycle
  where
    states = scanl (+) 0
    insertTillDuplicate seen (n:ns)
      | isDuplicate seen n = n
      | otherwise          = insertTillDuplicate (Set.insert n seen) ns
    insertTillDuplicate _ [] = error "counted to infinity and found nothing"
    isDuplicate seen n =
      Set.member n seen
