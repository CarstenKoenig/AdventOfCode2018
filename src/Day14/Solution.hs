{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications#-}
{-# LANGUAGE ScopedTypeVariables #-}

module Day14.Solution
  ( run
  , State (..)
  ) where

import           Control.Monad (zipWithM_)
import           Control.Monad.ST.Lazy (ST, strictToLazyST, runST)
import qualified Control.Monad.ST.Strict as StST
import           Data.Array.Base (readArray, writeArray, getBounds, newArray_)
import           Data.Array.MArray (MArray, newListArray)
import           Data.Array.ST (STUArray)
import           Data.List (isPrefixOf, tails, genericLength)
import           Data.Word (Word8)


data State arr = State
  { elf1      :: !Index
  , elf2      :: !Index
  , nrRecipes :: !Index
  , recipes   :: Recipes arr
  }


type Recipes arr = arr Index Recipe
type Recipe = Word8
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
start :: forall arr m . MArray arr Recipe m => m (State arr)
start = do
  recps <- newListArray (0,1) [3,7]
  pure $ State 0 1 2 recps


-- | looks for the first found index of the 'inpDigits' in all
-- the tails (of the infinite stream)
findInputIndex :: [Recipe] -> Int
findInputIndex =
  length . takeWhile (not . (inpDigits `isPrefixOf`)) . tails


-- | the inifinite stream of generated recipes
recipeList :: [Recipe]
recipeList = runST recipeListST


-- lazily generate the list of recipes using ST Monad
recipeListST :: forall arr s. (arr ~ STUArray s, MArray arr Recipe (StST.ST s)) => ST s [Recipe]
recipeListST = do
  st <- strictToLazyST (start @arr)
  (\ls -> 3:7:ls) <$> go st
  where
    go :: State arr -> ST s [Recipe]
    go st = do
      (added, st') <- strictToLazyST $ step st
      (added ++) <$> go st'


-- | generates the next state and the added recipes for that state
-- see the problem description for details on the procedure
step :: MArray arr Recipe m => State arr -> m ([Recipe], State arr)
step (State ind1 ind2 nrRec oldRecipes) = do
  (0, maxNr) <- getBounds oldRecipes
  let arrSz = maxNr + 1
  recipe1 <- readArray oldRecipes ind1
  recipe2 <- readArray oldRecipes ind2
  let added = combineRecipes recipe1 recipe2
  let newNr = nrRec + genericLength added
  let newSize = findSize arrSz newNr
  newRecipes <- if newSize <= arrSz then pure oldRecipes else grow newSize
  zipWithM_ (writeArray newRecipes) [nrRec..] added
  let stepForward ind sc = (ind + sc + 1) `mod` newNr
  pure $ (added, State
           (stepForward ind1 $ fromIntegral recipe1)
           (stepForward ind2 $ fromIntegral recipe2)
           newNr
           newRecipes)
  where
    findSize curSz minSz =
      head $ dropWhile (< minSz) $ iterate (* 2) curSz
    grow toSz = do
      arr <- newArray_ (0, toSz-1)
      mapM_ (\ind -> readArray oldRecipes ind >>= writeArray arr ind) [0..nrRec-1]
      pure arr


-- | combines to recipes into a new one
combineRecipes :: Recipe -> Recipe -> [Recipe]
combineRecipes sc1 sc2 =
  let (m,r) = (sc1+sc2) `divMod` 10
  in if m == 0 then [r] else [m, r]


-- the input recipes
inpDigits :: [Word8]
inpDigits = map (read .pure) $ show input
