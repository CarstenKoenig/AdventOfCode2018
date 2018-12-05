module Day5.Solution where

import           Data.Char (toLower, isLetter)
import           Data.List (minimumBy)
import           Data.Ord (comparing)


type Polymer = String


run :: IO ()
run = do
  putStrLn "DAY 5"
  polymer <- inputTxt
  let afterReaction = react polymer
  let initialLenght = length afterReaction
  putStrLn $ "part 1: " ++ show initialLenght
  let best = findBest afterReaction
  putStrLn $ "part 2: " ++ show (length best)


----------------------------------------------------------------------
-- Algorithm

-- | find the best (shortest) Polymer after improving with an element and then fullReaction again
findBest :: Polymer -> Polymer
findBest start =
  minimumBy (comparing length) [ improve c start | c <- ['a' .. 'z' ] ]


-- | "improves" an polymer by removing all occurences of the element or it's opposite polarity
improve :: Char -> Polymer -> Polymer
improve p = react . filter (\c -> toLower c /= p)


-- | reduce a Polymer to it's normal form
react :: Polymer -> Polymer
react = foldr reduce ""
  where
    reduce a (b:bs)
      | canReact a b = bs
    reduce a bs      = a : bs
    canReact a b     = a /= b && toLower a == toLower b


----------------------------------------------------------------------
-- IO

inputTxt :: IO Polymer
inputTxt = filter isLetter <$> readFile "./src/Day5/input.txt"


----------------------------------------------------------------------
-- helpers

-- | finds the fix point of the given function starting at
-- a point
fix :: Eq a => (a -> a) -> a -> a
fix f !x =
  let x' = f x
  in if x' == x then x else fix f x'
