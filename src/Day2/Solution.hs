module Day2.Solution where

import Data.List (sort, sortBy, group)
import Data.Ord (comparing)
import Data.Bifunctor (bimap)
import Data.Bool (bool)
import Data.Monoid (Sum(..))

run :: IO ()
run = do
  putStrLn "DAY 2"
  inp <- input
  putStrLn $ "part 1: " ++ show (part1 inp)


type BoxId = String

input :: IO [BoxId]
input = lines <$> readFile "./src/Day2/input.txt"


part1 :: [BoxId] -> Int
part1 = uncurry (*) . bimap getSum getSum . mconcat . map checkSum


checkSum :: BoxId -> (Sum Int, Sum Int)
checkSum boxId = score (groupId boxId)
  where
    score grp =
      bimap (bool 0 1) (bool 0 1) (containsTwoLetters grp, containsThreeLetters grp)


type Grouping = [(Char,Int)]


groupId :: BoxId -> Grouping
groupId = sortBy (comparing snd) . map (\gr@(c:_) -> (c, length gr)) . group . sort


containsTwoLetters :: Grouping -> Bool
containsTwoLetters = any (\(_,n) -> n == 2)


containsThreeLetters :: Grouping -> Bool
containsThreeLetters = any (\(_,n) -> n == 3)


