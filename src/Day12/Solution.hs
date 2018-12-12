{-# LANGUAGE TupleSections #-}
module Day12.Solution where

import Data.List (maximum)
import Data.Maybe (fromMaybe)
import Data.Monoid (All(..))
import Text.Parsec hiding (State)


data Input = Input
  { initial :: State
  , rules   :: [Rule]
  }


type Rule = [Bool] -> Maybe Bool
newtype State = State { unState :: [(Int, Bool)] }

instance Show State where
  show (State st) = map toC st
    where toC (_,True) = '#'
          toC (_,False) = '.'


----------------------------------------------------------------------
-- main
run :: IO ()
run = do
  putStrLn "DAY 12"
  inp <- inputTxt

  putStrLn $ "part 1: " ++ show (part1 inp)
  putStrLn $ "part 2: " ++ show (part2 inp 50000000000)


-- | steps through 20 generations and gives back
-- the 'filledPots' value as described in the problem
part1 :: Input -> Int
part1 = filledPots . afterNGens 20


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
  sum . map fst . filter (\(_,p) -> p) $ st


afterNGens :: Int -> Input -> State
afterNGens n inp = nTimes n step' (initial inp)
  where step' = step inp


-- | calculates the next generation of pots given a input and a state
step :: Input -> State -> State
step inp st'@(State st) = State $
  -- new pots might be generated left and right of the current row of filled pots
  -- add 4 empty ones so the fold can use them
  go $ (firstN-4,False) : (firstN-3,False) : (firstN-2,False) : (firstN-1,False)
       : st
       ++ [(lastN+1,False), (lastN+2, False), (lastN+3, False), (lastN+4, False)]
  where
    -- look at 5 pots at each time - the one in the mid is calculated
    go ((_,l1):(rest@((_,l2):(n,c):(_,r1):(_,r2):_))) =
      (n, ruleF inp [l1,l2,c,r1,r2]) : go rest
    -- if not enough pots remain stop
    go _ = []

    -- helpers
    firstN = firstPotNr st'
    lastN = lastPotNr st'
    firstPotNr = minimum . map fst . unState
    lastPotNr = maximum . map fst . unState


-- | collective rule - uses each given rule once after the others
ruleF :: Input -> [Bool] -> Bool
ruleF inp cur =
  getAll . fromMaybe (All False) . foldMap (\r -> All <$> r cur) $ rules inp


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


----------------------------------------------------------------------
-- helpers 

nTimes :: Int -> (a -> a) -> a -> a
nTimes 0  _ !x = x
nTimes !n f !x = nTimes (n-1) f (f x)

