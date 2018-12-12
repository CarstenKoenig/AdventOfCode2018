{-# LANGUAGE TupleSections #-}
module Day12.Solution where

import           Data.IntSet (IntSet)
import qualified Data.IntSet as S
import           Data.Maybe (fromMaybe)
import           Data.Monoid (First(..))
import           Text.Parsec hiding (State)


data Input = Input
  { initial :: State
  , rules   :: [Rule]
  }


-- | will the given pot be alive next generation given the state?
type Rule = PotNr -> State -> Maybe Bool

newtype State = State { unState :: IntSet }
  deriving Show

type PotNr = Int


insaneNumber :: Int
insaneNumber = 50000000000


----------------------------------------------------------------------
-- main
run :: IO ()
run = do
  putStrLn "DAY 12"
  inp <- inputTxt

  putStrLn $ "part 1: " ++ show (part1 inp)
  putStrLn $ "part 2: " ++ show (part2 inp insaneNumber)


-- | steps through 20 generations and gives back
-- the 'filledPots' value as described in the problem
part1 :: Input -> Int
part1 = filledPots . afterNGens 20


-- | this will run happiliy for quite some time
-- try your luck or move on to the "cheated" part 2
part2naive :: Input -> Int
part2naive = filledPots . afterNGens insaneNumber


-- | uses 'findConstantDiff' to look for constant diffs
-- and then calculates the total value by multiplying the
-- remaining generations by this diff
part2 :: Input -> Int -> Int
part2 inp gen =
  let (fromGen, genValue, diff) = findConstantDiff inp
  in genValue + (gen - fromGen) * diff
  -- or simply: 4698 + (gen - 102) * 46


----------------------------------------------------------------------
-- algorithm


-- | iterate the step function till it find a place where
-- 2 consecutive diffs of the 'filledPots' values give the
-- same value
-- from this place on it should stay constant
-- returns the (generation starting the cycle, the value of that generation, the diff-value from here)
findConstantDiff :: Input -> (Int, Int, Int)
findConstantDiff inp =
  findConst $ diffs numbers
  where
    numbers = zip [0..] $ map filledPots $ iterate (step inp) (initial inp)
    diffs ((_,lst):rest@((n,cur):_)) =
      (n,cur,cur-lst) : diffs rest
    diffs _ = error "unexpected case"
    findConst (f@(_,_,a):rest@((_,_,b):(_,_,c):_))
      | a == b && b == c = f
      | otherwise = findConst rest
    findConst _ = error "unexpected case"


filledPots :: State -> Int
filledPots (State st) =
  sum $ S.toList st


afterNGens :: Int -> Input -> State
afterNGens n inp = nTimes n step' (initial inp)
  where step' = step inp


-- | calculates the next generation of pots given a input and a state
step :: Input -> State -> State
step inp st = State $
  -- new pots might be generated left and right of the current row of filled pots
  -- add 4 empty ones so the fold can use them
  S.fromAscList . filter (combinedRules inp st) $ [firstPotNr-4..lastPotNr+4]
  where
    -- helpers
    firstPotNr = S.findMin $ unState st
    lastPotNr  = S.findMax $ unState st


-- | combines all the rules
-- assumes only one rule will match a given input pattern
-- the first one returning a value is used
combinedRules :: Input -> State -> PotNr -> Bool
combinedRules inp cur potNr =
  fromMaybe False . getFirst . foldMap (\rule -> First $ rule potNr cur) $ rules inp


----------------------------------------------------------------------
-- IO
inputTxt :: IO Input
inputTxt = parseInput <$> readFile "./src/Day12/input.txt"


inputTst :: IO Input
inputTst = parseInput <$> readFile "./src/Day12/test.txt"


----------------------------------------------------------------------
-- parsing
parseInput :: String -> Input
parseInput = either (error . show) id . parse inputP "input.txt"


type Parser a = Parsec String () a


inputP :: Parser Input
inputP = Input <$> initialP <*> many1 ruleP


initialP :: Parser State
initialP = State . S.fromList . map fst . filter snd . zip [0..]
           <$> (string "initial state: " *> boolListP <* many newline)


ruleP :: Parser Rule
ruleP = do
  left <- boolListP
  _ <- string " => "
  right <- boolP
  _ <- newline
  pure $ toRule left right
  where
    toRule bools outcome potNr (State set) =
      if all (\(offset, expected) -> (potNr + offset) `S.member` set == expected) offsets
      then Just outcome
      else Nothing
      where offsets = zip [-2..2] bools


boolListP :: Parser [Bool]
boolListP = many1 boolP


boolP :: Parser Bool
boolP = choice [ char '.' *> pure False, char '#' *> pure True ]


----------------------------------------------------------------------
-- helpers

nTimes :: Int -> (a -> a) -> a -> a
nTimes 0  _ !x = x
nTimes !n f !x = nTimes (n-1) f (f x)

