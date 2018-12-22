module Day22.Solution where


import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import Data.MemoTrie (memoFix)

-- | (X,Y
type Coord = (Int, Int)

data RegionType
  = Rocky
  | Narrow
  | Wet
  deriving (Show, Eq, Ord, Enum, Bounded)


type Depth = Int

data Input = Input
  { inputDepth :: Depth
  , inputTarget :: Coord
  } deriving Show


input :: Input
input = Input 5913 (8,701)


riskLevel :: Input -> Int
riskLevel inp =
  sum $ map riskLevelOf $ [(x,y) | x <- [0..toX], y <- [0..toY]]
  where
    riskLevelOf (x,y) =
      case regionType inp (x,y) of
        Rocky  -> 0
        Wet    -> 1
        Narrow -> 2
    (toX,toY) = inputTarget inp


caveMouthCoord :: Coord
caveMouthCoord = (0,0)


regionType :: Input -> Coord -> RegionType
regionType inp (x,y) =
  case erosionLevel inp (x,y) `mod` 3 of
    0 -> Rocky
    1 -> Wet
    _ -> Narrow


type ErosionLevel = Int

erosionLevel :: Input -> Coord -> ErosionLevel
erosionLevel inp = memoFix go
  where
    go       cont (x,y) = (geoIndex cont (x,y) + inputDepth inp) `mod` 20183
    geoIndex _    (0,0)                    = 0
    geoIndex _    c | c == inputTarget inp = 0
    geoIndex _    (x,0)                    = x * 16807
    geoIndex _    (0,y)                    = y * 48271
    geoIndex cont (x,y)                    = cont (x-1,y) * cont (x,y-1)



example :: Input
example = Input 510 (10,10)


drawMap :: Input -> (Int, Int) -> IO ()
drawMap inp (width, height) =
  mapM_ drawLine [0..height]
  where
    drawLine y = putStrLn $ map (formatRegion y) [0..width]
    formatRegion y x =
      case regionType inp (x,y) of
        Rocky  -> '.'
        Wet    -> '='
        Narrow -> '|'


run :: IO ()
run = do
  putStrLn "DAY 22"

  let inp = input

  putStrLn $ "part 1: " ++ show (riskLevel inp)
