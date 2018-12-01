module Main where

import qualified Day1.Solution as Day1
import           System.Console.ANSI (clearScreen)
import           System.IO (hSetBuffering, BufferMode(..), stdout)
import           Text.Read (readMaybe)

days :: [(Int, IO ())]
days =
  [ (1, Day1.run)
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


