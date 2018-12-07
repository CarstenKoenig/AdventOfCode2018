module Main where

import           Data.Maybe (listToMaybe)
import qualified Day1.Solution as Day1
import qualified Day2.Solution as Day2
import qualified Day3.Solution as Day3
import qualified Day4.Solution as Day4
import qualified Day5.Solution as Day5
import qualified Day6.Solution as Day6
import qualified Day7.Solution as Day7
import qualified Day8.Solution as Day8
import           System.Environment (getArgs)
import           System.IO (hSetBuffering, BufferMode(..), stdout)
import           Text.Read (readMaybe)
import Control.Monad ((>=>))

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
  ]
  where cont = (>>)


main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  dayPrg <- getDayProgramFromArgs
  case dayPrg of
    Just prg -> prg (return ())
    Nothing  -> queryProgram


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
