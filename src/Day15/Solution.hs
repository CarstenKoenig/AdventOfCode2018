{-# LANGUAGE FlexibleContexts #-}
module Day15.Solution
  (run
  , showGrid
  ) where

import           Control.Monad (foldM)
import           Data.List (sort)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe (listToMaybe, fromJust)
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
  let (n, end) = fromJust $ endState True grd
  in (n-1) * sum (map (getHitPoints . snd) . creatures $ end)
  where getHitPoints (_, hp, _) = hp


part2 :: String -> Int
part2 inp = test 4
  where
    test attPw =
      case endState False (parseInput attPw inp) of
        Just (n, end) ->
          (n-1) * sum (map (getHitPoints . snd) . creatures $ end)
          where getHitPoints (_, hp, _) = hp
        Nothing -> test (attPw + 1)


endState :: Bool -> Grid -> Maybe (Int, Grid)
endState mayElfDie grd =
  (\(n,(a,_)) -> (n,a)) <$> (safeHead . dropWhile (\(_,(a,b)) -> a /= b) $ zip [0..] $ zip rnds (tail rnds))
  where
    rnds = rounds mayElfDie grd
    safeHead [] = Nothing
    safeHead (x:_) = Just x



rounds :: Bool -> Grid -> [Grid]
rounds mayElfDie = iterateM (singleRound mayElfDie)


iterateM :: (a -> Maybe a) -> a -> [a]
iterateM f x =
  case f x of
    Nothing -> []
    Just x' -> x : iterateM f x'


singleRound :: Bool -> Grid -> Maybe Grid
singleRound mayElfDie grd = foldM unitActions grd $ map fst $ creatures grd
  where
    unitActions grd' coord =
      case M.lookup coord grd' of
        Just (Creature _ _ _) ->
          let (to, grd'') = move coord grd'
          in attack mayElfDie to grd''
        _ -> Just grd'


attack :: Bool -> Coord -> Grid -> Maybe Grid
attack mayElfDie from grd =
  case defs of
    (hp,at,ap'):_
      -- defender has enough hitpoints and survives
      | hp > ap   -> Just $ M.insert at (Creature defender (hp-ap) ap') grd
      -- elf dies
      | defender == Elf && not mayElfDie -> Nothing 
      -- defender dies
      | otherwise -> Just $ M.insert at Free grd
    _             -> Just $ grd
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

astarConfig :: Grid -> Coord -> AStar.Parameter Coord Coord
astarConfig grd from = AStar.Parameter
  (const 0)
  (freeNeighbours grd)
  (isAttackingPos grd attacker)
  id
  (\_ _ -> 1)
  where (Creature attacker _ _) = grd M.! from


enemy :: Creature -> Creature
enemy Goblin = Elf
enemy Elf = Goblin


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


run :: IO ()
run = do
  putStrLn "DAY 15"

  inp <- inputTxt

  let grd1 = parseInput 3 inp
  putStrLn $ "part 1: " ++ show (part1 grd1)

  putStrLn $ "part 2: " ++ show (part2 inp)


inputTxt :: IO String
inputTxt = readFile "./src/Day15/input.txt"


type Input = Grid


parseInput :: AttackPower -> String -> Input
parseInput elfAp = M.fromList . concat . zipWith (parseLine elfAp) [0..] . lines


parseLine :: AttackPower -> Int -> String -> [(Coord, Cell)]
parseLine elfAp y = zipWith (parseCell elfAp y) [0..]


parseCell :: AttackPower -> Int -> Int -> Char -> (Coord, Cell)
parseCell _ y x '#' = ((y,x), Wall)
parseCell _ y x 'G' = ((y,x), Creature Goblin 200 3)
parseCell elfAp y x 'E' = ((y,x), Creature Elf 200 elfAp)
parseCell _ y x '.' = ((y,x), Free) 
parseCell _ _ _ e   = error $ "unexpected cell-character: " ++ show e
