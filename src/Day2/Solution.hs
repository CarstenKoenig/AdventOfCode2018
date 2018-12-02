module Day2.Solution where

import Data.Bifunctor (bimap)
import Data.Bool (bool)
import Data.List (sort, sortBy, group)
import Data.Maybe (fromJust)
import Data.Monoid (Sum(..), First(..))
import Data.Ord (comparing)

run :: IO ()
run = do
  putStrLn "DAY 2"
  inp <- input
  putStrLn $ "part 1: " ++ show (part1 inp)
  putStrLn $ "part 2: " ++ show (part2 inp)


type BoxId = String


input :: IO [BoxId]
input = lines <$> readFile "./src/Day2/input.txt"


----------------------------------------------------------------------

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


----------------------------------------------------------------------

part2 :: [BoxId] -> BoxId
part2 = fromJust . findMatches


findMatches :: [BoxId] -> Maybe BoxId
findMatches [] = Nothing
findMatches (a:rest) =
  getFirst (mconcat $ (First . isMatch a <$> rest)) <> findMatches rest


isMatch :: BoxId -> BoxId -> Maybe BoxId
isMatch box1 box2
  | length com == length box1 - 1 = Just com
  | otherwise                     = Nothing
  where com = common box1 box2


common :: BoxId -> BoxId -> BoxId
common [] _ = ""
common _ [] = ""
common (a:as) (b:bs)
  | a == b    = a : common as bs
  | otherwise = common as bs
