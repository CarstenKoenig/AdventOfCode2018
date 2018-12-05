module Day5.Solution where

import Data.Char (isDigit)
import Data.Char (toUpper, isLower)
import Text.Parsec


type Polymer = String


run :: IO ()
run = do
  putStrLn "DAY 5"
  polymer <- inputTxt
  let afterReaction = fullReaction polymer
  putStrLn $ "part 1: " ++ show (length afterReaction)


inputTxt :: IO Polymer
inputTxt = filter (`elem` allowed) <$> readFile "./src/Day5/input.txt"


allowed :: String
allowed = ['a'..'z'] ++ ['A'..'Z']


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

fix :: Eq a => (a -> a) -> a -> a
fix f x =
  let x' = f x
  in if x' == x then x else fix f x'


test :: Polymer
test = "dabAcCaCBAcCcaDA"

-- parseInput :: String -> Input
-- parseInput = either (error . show) id . parse inputP "input.txt"


-- type Parser a = Parsec String () a


-- inputP :: Parser Input
-- inputP = pure ()
--   where
--     intP :: Parser Int
--     intP = read <$> many1 (satisfy isDigit)
