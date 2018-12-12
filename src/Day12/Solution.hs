{-# LANGUAGE TupleSections #-}
module Day12.Solution where

import Data.Bits as Bits
import Data.Char (isDigit)
import Text.Parsec hiding (State)
import Data.Maybe (fromJust, fromMaybe)
import Data.Monoid (All(..))
import Data.List (maximumBy, maximum)
import Data.Ord (comparing)
import Debug.Trace (traceShowId)


run :: IO ()
run = do
  putStrLn "DAY 12"
  inp <- inputTxt

  putStrLn $ "part 1: " ++ show (part1 inp)
  putStrLn $ "part 2: " ++ show (part2 inp 50000000000)

part1 :: Input -> Int
part1 = filledPots . afterNGens 20


part2 :: Input -> Int -> Int
part2 inp gen =
  let (fromGen, genValue, diff) = findConstantDiff inp
  in genValue + (gen - fromGen) * diff
  -- or simply: 4698 + (gen - 102) * 46


findConstantDiff :: Input -> (Int, Int, Int)
findConstantDiff inp =
  findConst $ diffs numbers
  where
    numbers = zip [0..] $ map filledPots $ iterate (step inp) (initial inp)
    diffs ((_,lst):rest@((n,cur):_)) = (n,cur,cur-lst) : diffs rest
    findConst (f@(_,_,a):rest@((_,_,b):(_,_,c):_))
      | a == b && b == c = f
      | otherwise = findConst rest


filledPots :: State -> Int
filledPots (State st) =
  sum . map fst . filter (\(_,p) -> p) $ st


afterNGens :: Int -> Input -> State
afterNGens n inp = nTimes n step' (initial inp)
  where step' = step inp


nTimes :: Int -> (a -> a) -> a -> a
nTimes 0  _ !x = x
nTimes !n f !x = nTimes (n-1) f (f x)


step :: Input -> State -> State
step inp st'@(State st) = State . go $ (firstN-4,False) : (firstN-3,False) : (firstN-2,False) : (firstN-1,False) : st ++ [(lastN+1,False), (lastN+2, False), (lastN+3, False), (lastN+4, False)]
  where
    go ((_,l1):(rest@((_,l2):(n,c):(_,r1):(_,r2):_))) =
      (n, rF [l1,l2,c,r1,r2]) : go rest
    go _ = []
    firstN = firstPotNr st'
    lastN = lastPotNr st'
    rF = ruleF inp
    firstPotNr (State ((nr,_):_)) = nr
    lastPotNr = maximum . map fst . unState


ruleF :: Input -> [Bool] -> Bool
ruleF inp cur =
  getAll . fromMaybe (All False) . foldMap (\r -> All <$> r cur) $ rules inp


inputTxt :: IO Input
inputTxt = parseInput <$> readFile "./src/Day12/input.txt"


inputTst :: IO Input
inputTst = parseInput <$> readFile "./src/Day12/test.txt"


data Input = Input
  { initial :: State
  , rules   :: [Rule]
  }

parseInput :: String -> Input
parseInput = either (error . show) id . parse inputP "input.txt"


type Parser a = Parsec String () a


newtype State = State { unState :: [(Int, Bool)] }
type Rule = [Bool] -> Maybe Bool


instance Show State where
  show (State st) = map toC st
    where toC (_,True) = '#'
          toC (_,False) = '.'


inputP :: Parser Input
inputP = Input <$> initialP <*> many1 ruleP


initialP :: Parser State
initialP = State . zip [0..] <$> (string "initial state: " *> boolListP <* many newline)

ruleP :: Parser Rule
ruleP = do
  left <- boolListP
  _ <- string " => "
  right <- boolP
  _ <- newline
  pure $ \inp -> if and (zipWith (==) inp left) then Just right else Nothing


boolListP :: Parser [Bool]
boolListP = many1 boolP


boolP :: Parser Bool
boolP = choice [ char '.' *> pure False, char '#' *> pure True ]
