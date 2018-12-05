{-# LANGUAGE BangPatterns #-}
module Day5.Solution where

import Data.Char (toUpper, isLower)
import Data.List (minimumBy)
import Data.Ord (comparing)


type Polymer = String


run :: IO ()
run = do
  putStrLn "DAY 5"
  polymer <- inputTxt
  let afterReaction = fullReaction polymer
  let initialLenght = length afterReaction
  putStrLn $ "part 1: " ++ show initialLenght
  let best = findBest afterReaction
  putStrLn $ "part 2: " ++ show (length best)


improve :: Char -> Polymer -> Polymer
improve p = fullReaction . filter (\c -> c /= p && c /= toUpper p)


findBest :: Polymer -> Polymer
findBest start =
  minimumBy (comparing length) [ improve c start | c <- ['a' .. 'z' ] ]


fullReaction :: Polymer -> Polymer
fullReaction = fix react


react :: Polymer -> Polymer
react (a:b'@(b:rest))
  | canReact a b = react rest
  | otherwise    = a : react b'
react xs = xs


canReact :: Char -> Char -> Bool
canReact a a' =
  toUpper a == toUpper a' && isLower a /= isLower a'


inputTxt :: IO Polymer
inputTxt = filter (`elem` allowed) <$> readFile "./src/Day5/input.txt"


allowed :: String
allowed = ['a'..'z'] ++ ['A'..'Z']

fix :: Eq a => (a -> a) -> a -> a
fix f !x =
  let x' = f x
  in if x' == x then x else fix f x'

