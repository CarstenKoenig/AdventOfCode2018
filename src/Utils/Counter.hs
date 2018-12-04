module Utils.Counter
  ( Counter
  , empty
  , Utils.Counter.maximum, Utils.Counter.minimum
  , desc, asc
  , add, incr
  , fromList
  )
where

import           Data.List (sortBy)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Ord (comparing)

data Counter key n = Counter { getMap :: Map key n }


empty :: Counter key n
empty = Counter Map.empty


maximum :: Ord n => Counter key n -> (key, n)
maximum = head . desc


minimum :: Ord n => Counter key n -> (key, n)
minimum = head . asc


desc :: Ord n => Counter key n -> [(key, n)]
desc = sortBy (flip $ comparing snd) . Map.toList . getMap


asc :: Ord n => Counter key n -> [(key, n)]
asc = sortBy (comparing snd) . Map.toList . getMap


add :: Ord key => Num n => key -> n -> Counter key n -> Counter key n
add key n = Counter . Map.insertWith (+) key n . getMap


incr :: Ord key => Num n => key -> Counter key n -> Counter key n
incr key = add key 1


fromList :: Ord key => Num n => [(key, n)] -> Counter key n
fromList = foldr (\(k,n) -> add k n) empty
