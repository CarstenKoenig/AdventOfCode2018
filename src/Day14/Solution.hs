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
endsWithInput st = checkDigits (recipes st)
  where
    checkDigits S.Empty = False
    checkDigits s@(r S.:|> _) = checkEnd inpDigits s || checkEnd inpDigits r
    checkEnd S.Empty _ = True
    checkEnd _ S.Empty = False
    checkEnd (restA S.:|> a) (restB S.:|> b) = a == b && checkEnd restA restB


step :: State -> State
step st =
  st { elf1 = stepForward (elf1 st) recipe1
     , elf2 = stepForward (elf2 st) recipe2
     , recipes = newRecipes
     }
  where
    newRecipes = recipes st S.>< added
    added      = combineRecipes recipe1 recipe2
    recipe1    = fromJust $ recipes st S.!? elf1 st
    recipe2    = fromJust $ recipes st S.!? elf2 st
    nrRecs     = nrRecipes st
    stepForward ind sc = (ind + sc + 1) `mod` (nrRecs + length added)


nrRecipes :: State -> Int
nrRecipes = S.length . recipes


combineRecipes :: Score -> Score -> Seq Score
combineRecipes sc1 sc2 =
  let (m,r) = (sc1+sc2) `divMod` 10
  in if m == 0 then S.singleton r else S.fromList [m, r]


inpDigits :: Seq Int
inpDigits = digits input


digits :: Int -> Seq Int
digits = S.fromList . map (read . pure) . show


run :: IO ()
run = do
  putStrLn "DAY 14"
  putStrLn $ "part 1: " ++ part1
  putStrLn $ "part 2: " ++ show part2

type Input = Int

input :: Input
input = 894501
