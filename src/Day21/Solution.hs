{-# LANGUAGE TypeApplications #-}
module Day21.Solution where

import           Data.Bits (Bits(..))
import qualified Data.Set as Set
import Data.Word (Word32)


run :: IO ()
run = do
  putStrLn "DAY 21"
  putStrLn $ "part 1: " ++ show (firstReg5 @Word32)
  putStrLn $ "part 2: " ++ show (lookForRepeat @Word32)


step :: (Integral a, Bits a, Ord a, Num a) => a -> a
step n = go konstante1 $ n .|. 0x10000
  where
    go !reg5 !reg2 =
      let reg5' = to24bit $ (to24bit $ reg5 + (reg2 .&. 0xff)) * konstante2
      in if 256 > reg2 then reg5' else go reg5' (reg2 `div` 256)
    to24bit x = x .&. 0xffffff
    konstante1 = 3935295
    konstante2 = 65899


firstReg5 :: (Integral a, Bits a, Ord a, Num a) => a
firstReg5 = step 0


lookForRepeat :: (Integral a, Bits a, Ord a, Num a) => a
lookForRepeat =
  go Set.empty 0
  where
    go seen n =
      let n' = step n
      in if n' `Set.member` seen
         then n
         else go (Set.insert n' seen) n'
