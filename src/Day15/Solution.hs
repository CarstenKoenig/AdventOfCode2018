{-# LANGUAGE FlexibleContexts #-}
module Day15.Solution where

import           Control.Monad.Trans.Maybe (runMaybeT)
import           Data.Char (isDigit)
import           Data.List (foldl')
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe (catMaybes, mapMaybe, fromMaybe, listToMaybe)
import           Data.Ord (comparing)
import           Data.Tuple (swap)
import           Text.Parsec
import qualified Utils.Astar as AStar


data Cell
  = Free
  | Creature Creature
  | Wall
  deriving (Eq, Show)


data Creature
  = Elf
  | Goblin
  deriving (Eq, Show)


-- | Coord with (y,x) (Row/Column)
type Coord = (Int, Int)

type Grid = Map Coord Cell


enemyPoss :: Grid -> Creature -> [(Coord)]
enemyPoss grd attacker =
  map fst $ filter (isEnemy attacker . snd) $ creatures grd


creatures :: Grid -> [(Coord, Creature)]
creatures = M.toAscList . M.mapMaybe getCreature
  where getCreature (Creature c) = Just c
        getCreature _            = Nothing


isFree :: Grid -> Coord -> Bool
isFree grd coord =
  case M.lookup coord grd of
    Just Free -> True
    _         -> False


isCreature :: Grid -> Creature -> Coord -> Bool
isCreature grd creature coord =
  case M.lookup coord grd of
    Just (Creature c) -> c == creature
    _                 -> False


coords :: Grid -> [Coord]
coords = M.keys


doMoves :: Grid -> Grid
doMoves grd = foldl' (flip move) grd $ map fst $ creatures grd


move :: Coord -> Grid -> Grid
move from grd =
  case findMove grd from of
    Nothing -> grd
    Just to -> M.insert to c $ M.insert from Free $ grd
  where c = grd M.! from


findMove :: Grid -> Coord -> Maybe Coord
findMove grd from =
  listToMaybe $ drop 1 $ AStar.aStar params from
  where params = astarConfig grd from


astarConfig :: Grid -> Coord -> AStar.Parameter Coord Coord
astarConfig grd from = AStar.Parameter
  (stepsToNearestEnemy grd attacker)
  (freeNeighbours grd)
  (isAttackingPos grd attacker)
  id
  (\_ _ -> 1)
  where (Creature attacker) = grd M.! from


stepsToNearestEnemy :: Grid -> Creature -> Coord -> Int
stepsToNearestEnemy grd attacker curPos =
  pred . minimum $ map (dist curPos) $ enemyPoss grd attacker


enemy :: Creature -> Creature
enemy Goblin = Elf
enemy Elf = Goblin


isEnemy :: Creature -> Creature -> Bool
isEnemy ofCreat other = other == enemy ofCreat


freeNeighbours :: Grid -> Coord -> [Coord]
freeNeighbours grd (r,c) =
  filter (isFree grd) [ (r-1,c), (r,c-1), (r,c+1), (r+1,c) ]


isAttackingPos :: Grid -> Creature -> Coord -> Bool
isAttackingPos grd attacker (r,c) =
  any (isCreature grd defender) [ (r-1,c), (r,c-1), (r,c+1), (r+1,c) ]
  where defender = enemy attacker


dist :: Coord -> Coord -> Int
dist (r,c) (r',c') = abs (r'-r) + abs (c'-c)


run :: IO ()
run = do
  putStrLn "DAY 15"


inputFile :: FilePath -> IO Input
inputFile path = parseInput <$> readFile ("./src/Day15/" ++ path)


inputTxt :: IO Input
inputTxt = parseInput <$> readFile "./src/Day15/input.txt"


type Input = Grid


parseInput :: String -> Input
parseInput = M.fromList . concat . zipWith parseLine [0..] . lines


parseLine :: Int -> String -> [(Coord, Cell)]
parseLine y = zipWith (parseCell y) [0..]


parseCell :: Int -> Int -> Char -> (Coord, Cell)
parseCell y x '#' = ((y,x), Wall)
parseCell y x 'G' = ((y,x), Creature Goblin)
parseCell y x 'E' = ((y,x), Creature Elf)
parseCell y x '.' = ((y,x), Free) 
parseCell _ _ e   = error $ "unexpected cell-character: " ++ show e
