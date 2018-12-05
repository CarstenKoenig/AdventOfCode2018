module Main where

import qualified Day1.Solution as Day1
import qualified Day2.Solution as Day2
import qualified Day3.Solution as Day3
import qualified Day4.Solution as Day4
import qualified Day5.Solution as Day5
import qualified Day6.Solution as Day6
import           System.Console.ANSI (clearScreen)
import           System.IO (hSetBuffering, BufferMode(..), stdout)
import           Text.Read (readMaybe)

days :: [(Int, IO ())]
days =
  [ (1, Day1.run)
  , (2, Day2.run)
  , (3, Day3.run)
  , (4, Day4.run)
  , (5, Day5.run)
  , (6, Day6.run)
  ]


main :: IO ()
main = do
  clearScreen
  hSetBuffering stdout NoBuffering
  loop
  where
    getDay = (`lookup` days)
    loop = do
      putStr "which day do you want to run? "
      dayNr <- readMaybe <$> getLine
      case dayNr >>= getDay of
        Just found -> do
          clearScreen
          found
        Nothing    -> do
          putStrLn "Day not found (yet)"
          loop


