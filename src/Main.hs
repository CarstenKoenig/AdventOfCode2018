module Main where

import           Control.Monad ((>=>))
import           Data.Maybe (fromJust)
import           Data.Maybe (listToMaybe)
import qualified Day1.Solution as Day1
import qualified Day10.Solution as Day10
import qualified Day11.Solution as Day11
import qualified Day12.Solution as Day12
import qualified Day13.Solution as Day13
import qualified Day14.Solution as Day14
import qualified Day15.Solution as Day15
import qualified Day16.Solution as Day16
import qualified Day17.Solution as Day17
import qualified Day18.Solution as Day18
import qualified Day19.Solution as Day19
import qualified Day2.Solution as Day2
import qualified Day20.Solution as Day20
import qualified Day21.Solution as Day21
import qualified Day22.Solution as Day22
import qualified Day23.Solution as Day23
import qualified Day24.Solution as Day24
import qualified Day25.Solution as Day25
import qualified Day3.Solution as Day3
import qualified Day4.Solution as Day4
import qualified Day5.Solution as Day5
import qualified Day6.Solution as Day6
import qualified Day7.Solution as Day7
import qualified Day8.Solution as Day8
import qualified Day9.Solution as Day9
import           System.Environment (getArgs)
import           System.IO (hSetBuffering, BufferMode(..), stdout)
import           Text.Read (readMaybe)

maxDay :: Int
maxDay = 25

days :: [(Int, IO () -> IO ())]
days =
  [ (0, const (return ()))
  , (1, cont Day1.run)
  , (2, cont Day2.run)
  , (3, cont Day3.run)
  , (4, cont Day4.run)
  , (5, cont Day5.run)
  , (6, cont Day6.run)
  , (7, cont Day7.run)
  , (8, cont Day8.run)
  , (9, cont Day9.run)
  , (10, cont Day10.run)
  , (11, cont Day11.run)
  , (12, cont Day12.run)
  , (13, cont Day13.run)
  , (14, cont Day14.run)
  , (15, cont Day15.run)
  , (16, cont Day16.run)
  , (17, cont Day17.run)
  , (18, cont Day18.run)
  , (19, cont Day19.run)
  , (20, cont Day20.run)
  , (21, cont Day21.run)
  , (22, cont Day22.run)
  , (23, cont Day23.run)
  , (24, cont Day24.run)
  , (25, cont Day25.run)
  , (99, const runAll)
  ]
  where cont = (>>)


main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  dayPrg <- getDayProgramFromArgs
  case dayPrg of
    Just prg -> prg (return ())
    Nothing  -> queryProgram


runAll :: IO ()
runAll = go maxDay
  where
    go n = fromJust (getDay n) (go $ n - 1)


getDayProgramFromArgs :: IO (Maybe (IO () -> IO ()))
getDayProgramFromArgs =
  (listToMaybe >=> readMaybe >=> getDay) <$> getArgs


getDay :: Int -> Maybe (IO () -> IO ())
getDay = (`lookup` days)


queryProgram :: IO ()
queryProgram = loop
  where
    loop = do
      putStr "which day do you want to run (0 to quit)? "
      dayNr <- readMaybe <$> getLine
      case dayNr >>= getDay of
        Just found -> do
          found loop
        Nothing    -> do
          putStrLn "Day not found (yet)"
          loop
