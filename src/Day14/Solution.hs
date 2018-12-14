module Day14.Solution
  ( run
  ) where

import           Data.Maybe (fromMaybe, fromJust)
import           Data.Sequence (Seq)
import qualified Data.Sequence as S
import Data.List (isPrefixOf, tails)


type Recipes = Seq Score

type Score = Int
type Index = Int


data State = State
  { elf1    :: !Index
  , elf2    :: !Index
  , recipes :: Recipes
  } deriving Show


run :: IO ()
run = do
  putStrLn "DAY 14"
  putStrLn $ "part 1: " ++ part1
  putStrLn $ "part 2: " ++ show part2


part1 :: String
part1 = concatMap show $ take 10 $ drop input recipeList

part2 :: Int
part2 = findInputIndex recipeList


start :: State
start = State 0 1 (S.fromList [3,7])


findInputIndex :: [Score] -> Int
findInputIndex =
  length . takeWhile (not . (inpDigits `isPrefixOf`)) . tails


recipeList :: [Score]
recipeList = 3 : 7 : go start
  where
    go st =
      let (added, st') = step st
      in added ++ go st'


step :: State -> ([Score], State)
step (State ind1 ind2 oldRecipes) = (added, State
  (stepForward ind1 recipe1)
  (stepForward ind2 recipe2)
  newRecipes)
  where
    newRecipes = oldRecipes S.>< S.fromList added
    added      = combineRecipes recipe1 recipe2
    recipe1    = oldRecipes `S.index` ind1
    recipe2    = oldRecipes `S.index` ind2
    stepForward ind sc = (ind + sc + 1) `mod` (S.length newRecipes)


combineRecipes :: Score -> Score -> [Score]
combineRecipes sc1 sc2 =
  let (m,r) = (sc1+sc2) `divMod` 10
  in if m == 0 then [r] else [m, r]


inpDigits :: [Int]
inpDigits = digits input


digits :: Int -> [Int]
digits =  map (read . pure) . show


type Input = Int

input :: Input
input = 894501
