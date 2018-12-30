{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
module Day23.Z3
  ( Day23.Z3.run
  ) where

import           Data.SBV
import           Day23.Solution


----------------------------------------------------------------------
-- main

run :: IO ()
run = do
  putStrLn "DAY 23 - using Z3"
  inp <- inputTxt

  part2 inp


part2 :: Input -> IO ()
part2 inp = do
  -- optimize and make sure that the "bots in range" goal is more important than "distance from origin"
  res <- optimize Lexicographic $ script inp
  print res


script :: Input -> Goal
script bots = do
  xV <- sInteger "x"
  yV <- sInteger "y"
  zV <- sInteger "z"

  maximize "bots in range" $ sum $ map (isInRange (xV,yV,zV)) bots
  minimize "distance from origin" $ mkAbs xV + mkAbs yV + mkAbs zV

  where
    -- 1 if xV,yV,zV is in range of the given bot - 0 otherwise
    isInRange (xVar, yVar, zVar) bot =
      ite (distFrom (xVar, yVar, zVar) bot .<= fromIntegral (nanobotRadius bot)) 1 (0 :: SInteger)
    -- manhatten distance
    distFrom (xVar, yVar, zVar) bot =
      let (x',y',z') = nanobotPos bot
      in mkAbs (xVar - fromIntegral x') + mkAbs (yVar - fromIntegral y') + mkAbs (zVar - fromIntegral z')
    mkAbs x = ite (0 .< x) x (negate x)


{- the result is this:

DAY 23 - using Z3
Optimal model:
  x                    =  15189637 :: Integer
  y                    =  49255060 :: Integer
  z                    =  48552937 :: Integer
  bots in range        =       968 :: Integer
  distance from origin = 112997634 :: Integer

which is the right answer (the last number)
-}
