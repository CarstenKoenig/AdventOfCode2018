module Utils.Counter
  ( Counter
  , empty, size, keys
  , Utils.Counter.maximum, Utils.Counter.minimum
  , desc, asc
  , add, incr, decr
  , fromList
  , remove
  )
where

import           Data.List (sortBy)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Ord (comparing)

data Counter key n = Counter { getMap :: Map key n }
  deriving Show


empty :: Counter key n
empty = Counter Map.empty


size :: Counter key n -> Int
size (Counter m) = Map.size m


keys :: Counter key n -> [key]
keys (Counter m) = Map.keys m


maximum :: Ord n => Counter key n -> (key, n)
maximum = head . desc


minimum :: Ord n => Counter key n -> (key, n)
minimum = head . asc


desc :: Ord n => Counter key n -> [(key, n)]
desc = sortBy (flip $ comparing snd) . Map.toList . getMap


asc :: Ord n => Counter key n -> [(key, n)]
asc = sortBy (comparing snd) . Map.toList . getMap


add :: Ord key => Num n => Ord n => key -> n -> Counter key n -> Counter key n
add key n = Counter . Map.insertWith (\n' o -> max (n'+o) 0) key n . getMap


incr :: Ord key => Num n => Ord n => key -> Counter key n -> Counter key n
incr key = add key 1


decr :: Ord key => Num n => Ord n => key -> Counter key n -> Counter key n
decr key = add key (-1)


fromList :: Ord key => Num n => Ord n => [(key, n)] -> Counter key n
fromList = foldr (\(k,n) -> add k n) empty


remove :: Ord key => key -> Counter key n -> Counter key n
remove k (Counter m) = Counter (Map.delete k m)
