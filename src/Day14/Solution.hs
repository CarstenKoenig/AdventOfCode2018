module Day14.Solution
  ( run
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)


type Recipes = Map Index Score

type Score = Int
type Index = Int


data State = State
  { elf1    :: Index
  , elf2    :: Index
  , recipes :: Recipes
  } deriving Show


part1 :: String
part1 = concatMap show $ tenAfter start input


-- should be 20365081
part2 :: Int
part2 = M.size (recipes found) - length tgt - off
  where
    found = findEndsWith
    off = if endsWith tgt 0 found then 0 else 1
    tgt = show input


start :: State
start = State 0 1 (M.fromList $ zip [0..] [3,7])


tenAfter :: State -> Int -> [Int]
tenAfter state n =
  map snd .take 10 . drop n . M.toList . recipes . head . dropWhile (\st -> M.size (recipes st) < 10 + n) $ iterate step state


showRecipes :: State -> String
showRecipes = concatMap show . map snd . M.toList . recipes


endsWith :: String -> Int -> State -> Bool
endsWith tgt off st = and $ zipWith (==) (reverse tgt) (drop off $ reverse $ showRecipes st)


findEndsWith :: State
findEndsWith = head $ dropWhile (not . endsWithInput) $ iterate step start


endsWithInput :: State -> Bool
endsWithInput st = checkDigits 0 || checkDigits 1
  where
    checkDigits off = and $ zipWith (==) [digitAt ind | ind <- [ recpSz - inpSize - off..]] inpDigits
    digitAt ind = fromMaybe (-1) $ M.lookup ind (recipes st)
    inpDigits = digits input
    inpSize = length inpDigits
    recpSz = M.size (recipes st)


step :: State -> State
step st =
  st { elf1 = stepForward (elf1 st) recipe1
     , elf2 = stepForward (elf2 st) recipe2
     , recipes = newRecipes
     }
  where
    newRecipes = M.union (recipes st) $ M.fromList $ zip [nrRecs..] added
    added = combineRecipes recipe1 recipe2
    recipe1 = recipes st M.! elf1 st
    recipe2 = recipes st M.! elf2 st
    nrRecs = nrRecipes st
    stepForward ind sc = (ind + sc + 1) `mod` (nrRecs + length added)


nrRecipes :: State -> Int
nrRecipes = M.size . recipes


combineRecipes :: Score -> Score -> [Score]
combineRecipes sc1 sc2 = digits $ sc1 + sc2


digits :: Int -> [Int]
digits = map (read . pure) . show


run :: IO ()
run = do
  putStrLn "DAY 14"
  putStrLn $ "part 1: " ++ part1
  putStrLn $ "part 2: " ++ show part2

type Input = Int

input :: Input
input = 894501
