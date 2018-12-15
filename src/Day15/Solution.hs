{-# LANGUAGE FlexibleContexts #-}
module Day15.Solution where

import           Control.Monad.Trans.Maybe (runMaybeT)
import           Data.Char (isDigit)
import           Data.List (foldl', sort)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe (catMaybes, mapMaybe, fromMaybe, listToMaybe)
import           Data.Ord (comparing)
import           Data.Tuple (swap)
import           Text.Parsec
import qualified Utils.Astar as AStar


data Cell
  = Free
  | Creature Creature HitPoints AttackPower
  | Wall
  deriving (Eq, Show)


data Creature
  = Elf
  | Goblin
  deriving (Eq, Show)

type AttackPower = Int
type HitPoints = Int

-- | Coord with (y,x) (Row/Column)
type Coord = (Int, Int)

type Grid = Map Coord Cell


showGrid :: Grid -> IO ()
showGrid grd = do
  let (y0,x0) = minimum $ coords grd
  let (y1,x1) = maximum $ coords grd
  mapM_ (showLine [x0..x1]) [y0..y1]
  where
    showLine xs y =
      putStrLn $ map showContent xs
      where
        showContent x =
          case grd M.! (y,x) of
            Free -> '.'
            Creature Goblin _ _ -> 'G'
            Creature Elf _ _ -> 'E'
            Wall -> '#'


enemyPoss :: Grid -> Creature -> [(Coord)]
enemyPoss grd attacker =
  map fst $ filter (isEnemy attacker . getCreature . snd) $ creatures grd
  where getCreature (c,_,_) = c


creatures :: Grid -> [(Coord, (Creature, HitPoints, AttackPower))]
creatures = M.toAscList . M.mapMaybe getCreature
  where getCreature (Creature c hp ap ) = Just (c, hp, ap)
        getCreature _                   = Nothing


isFree :: Grid -> Coord -> Bool
isFree grd coord =
  case M.lookup coord grd of
    Just Free -> True
    _         -> False


isCreature :: Grid -> Creature -> Coord -> Bool
isCreature grd creature coord =
  case M.lookup coord grd of
    Just (Creature c _ _) -> c == creature
    _                     -> False


coords :: Grid -> [Coord]
coords = M.keys


part1 :: Grid -> Int
part1 grd =
  let (n, end) = endState grd
  in (n-1) * sum (map (getHitPoints . snd) . creatures $ end)
  where getHitPoints (_, hp, _) = hp


endState :: Grid -> (Int, Grid)
endState grd = (\(n,(a,_)) -> (n,a)) . head . dropWhile (\(_,(a,b)) -> a /= b) $ zip [0..] $ zip rnds (tail rnds)
  where rnds = rounds grd


rounds :: Grid -> [Grid]
rounds = iterate singleRound


singleRound :: Grid -> Grid
singleRound grd = foldl' unitActions grd $ map fst $ creatures grd
  where
    unitActions grd' coord =
      case M.lookup coord grd' of
        Just (Creature _ _ _) ->
          let (to, grd'') = move coord grd'
          in attack to grd''
        _ -> grd'


attack :: Coord -> Grid -> Grid
attack from grd =
  case defs of
    (hp,at,ap'):_
      -- defender has enough hitpoints and survives
      | hp > ap   -> M.insert at (Creature defender (hp-ap) ap') grd
      -- defender dies
      | otherwise -> M.insert at Free grd
    _             -> grd
  where
    (Creature attacker _ ap) = grd M.! from
    defender = enemy attacker
    defs = defenders grd attacker from


move :: Coord -> Grid -> (Coord, Grid)
move from grd =
  case findMove grd from of
    Nothing -> (from, grd)
    Just to -> (to, M.insert to c $ M.insert from Free $ grd)
  where c = grd M.! from


findMove :: Grid -> Coord -> Maybe Coord
findMove grd from =
  listToMaybe $ drop 1 $ AStar.aStar params from
  where params = astarConfig grd from

findMoves :: Grid -> Coord -> [Coord]
findMoves grd from =
  AStar.aStar params from
  where params = astarConfig grd from


astarConfig :: Grid -> Coord -> AStar.Parameter Coord Coord
astarConfig grd from = AStar.Parameter
  (const 0)
  (freeNeighbours grd)
  (isAttackingPos grd attacker)
  id
  (\_ _ -> 1)
  where (Creature attacker _ _) = grd M.! from


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

defenders :: Grid -> Creature -> Coord -> [(HitPoints, Coord, AttackPower)]
defenders grd attacker (r,c) =
  sort . map addStats . filter (isCreature grd defender) $ [ (r-1,c), (r,c-1), (r,c+1), (r+1,c) ]
  where defender = enemy attacker
        addStats coord =
          let (Creature _ hp ap) = grd M.! coord
          in (hp, coord, ap)


dist :: Coord -> Coord -> Int
dist (r,c) (r',c') = abs (r'-r) + abs (c'-c)


run :: IO ()
run = do
  putStrLn "DAY 15"

  grd <- inputTxt
  putStrLn $ "part 1: " ++ show (part1 grd)


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
parseCell y x 'G' = ((y,x), Creature Goblin 200 3)
parseCell y x 'E' = ((y,x), Creature Elf 200 3)
parseCell y x '.' = ((y,x), Free) 
parseCell _ _ e   = error $ "unexpected cell-character: " ++ show e
