module Day14.Solution
  ( run
  ) where

import           Data.Maybe (fromMaybe, fromJust)
import           Data.Sequence (Seq)
import qualified Data.Sequence as S


type Recipes = Seq Score

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
part2 = S.length (recipes found) - length tgt - off
  where
    found = findEndsWith
    off = if endsWith tgt 0 found then 0 else 1
    tgt = show input


start :: State
start = State 0 1 (S.fromList [3,7])


tenAfter :: State -> Int -> Seq Int
tenAfter state n =
  S.take 10 . S.drop n . recipes . head . dropWhile (\st -> S.length (recipes st) < 10 + n) $ iterate step state


showRecipes :: State -> String
showRecipes = concatMap show . recipes


endsWith :: String -> Int -> State -> Bool
endsWith tgt off st = and $ zipWith (==) (reverse tgt) (drop off $ reverse $ showRecipes st)


findEndsWith :: State
findEndsWith = head $ dropWhile (not . endsWithInput) $ iterate step start


endsWithInput :: State -> Bool
endsWithInput st = checkDigits 0 || checkDigits 1
  where
    checkDigits off = and $ zipWith (==) [digitAt ind | ind <- [ recpSz - inpSize - off..]] inpDigits
    digitAt ind = fromMaybe (-1) $ S.lookup ind (recipes st)
    inpDigits = digits input
    inpSize = length inpDigits
    recpSz = nrRecipes st


step :: State -> State
step st =
  st { elf1 = stepForward (elf1 st) recipe1
     , elf2 = stepForward (elf2 st) recipe2
     , recipes = newRecipes
     }
  where
    newRecipes = recipes st S.>< added
    added = S.fromList $ combineRecipes recipe1 recipe2
    recipe1 = fromJust $ recipes st S.!? elf1 st
    recipe2 = fromJust $ recipes st S.!? elf2 st
    nrRecs = nrRecipes st
    stepForward ind sc = (ind + sc + 1) `mod` (nrRecs + length added)


nrRecipes :: State -> Int
nrRecipes = S.length . recipes


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
