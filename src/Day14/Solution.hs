module Day14.Solution
  ( run
  ) where

import           Data.Sequence (Seq)
import qualified Data.Sequence as S
import Data.List (isPrefixOf, tails)


data State = State
  { elf1    :: !Index
  , elf2    :: !Index
  , recipes :: Recipes
  } deriving Show

type Recipes = Seq Recipe
type Recipe = Int
type Index = Int

type Input = Int

-- | my input for todays problem
input :: Input
input = 894501

----------------------------------------------------------------------
-- main

run :: IO ()
run = do
  putStrLn "DAY 14"
  putStrLn $ "part 1: " ++ part1
  putStrLn $ "part 2: " ++ show part2


part1 :: String
part1 = concatMap show $ take 10 $ drop input recipeList

part2 :: Int
part2 = findInputIndex recipeList


----------------------------------------------------------------------
-- algorithm

-- | starting state - two recipes 3 and 7
start :: State
start = State 0 1 (S.fromList [3,7])


-- | looks for the first found index of the 'inpDigits' in all
-- the tails (of the infinite stream)
findInputIndex :: [Recipe] -> Index
findInputIndex =
  length . takeWhile (not . (inpDigits `isPrefixOf`)) . tails


-- | the inifinite stream of generated recipes
recipeList :: [Recipe]
recipeList = 3 : 7 : go start
  where
    go st =
      let (added, st') = step st
      in added ++ go st'


-- | generates the next state and the added recipes for that state
-- see the problem description for details on the procedure
step :: State -> ([Recipe], State)
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


-- | combines to recipes into a new one
combineRecipes :: Recipe -> Recipe -> [Recipe]
combineRecipes sc1 sc2 =
  let (m,r) = (sc1+sc2) `divMod` 10
  in if m == 0 then [r] else [m, r]


-- the input recipes
inpDigits :: [Recipe]
inpDigits = map (read .pure) $ show input
