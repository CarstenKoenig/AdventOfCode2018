{-# LANGUAGE FlexibleContexts #-}
module Day15.Solution
  (run
  , showGrid
  ) where

import           Control.Concurrent (threadDelay)
import           Control.Monad (foldM)
import           Data.List (sort)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe (listToMaybe, fromJust)
import           System.Console.ANSI (clearScreen)
import qualified Utils.Astar as AStar


-- | a map of 'Coord' to their content 'Cell'
type Grid = Map Coord Cell

-- | a Cell in the game
data Cell
  -- | empty - nothing here
  = Free
  -- | this is blocked by a wall
  | Wall
  -- | a creatures with the given type, hitpoints and attackpower occupies this cell
  | Creature Creature HitPoints AttackPower
  deriving (Eq, Show)


-- | the type of a creature
data Creature
  -- | a nice little elf helping Santa
  = Elf
  -- | a bad Goblin wanting nothing else than to kill Elves and eat their sweets
  | Goblin
  deriving (Eq, Show)


-- | the attack-power of a creature
-- if it attacks another creature this
-- will deal damage to the other creatures
-- 'HitPoints'
type AttackPower = Int


-- | the hitpoints of a creature
-- if the hitpoints reach 0 the creature dies
type HitPoints = Int


-- | Coord with (y,x) (Row/Column)
type Coord = (Int, Int)


-- | creates a grid where the elves have the given 'AttackPower'
type Input = AttackPower -> Grid


----------------------------------------------------------------------
-- main

run :: IO ()
run = do
  putStrLn "DAY 15"

  makeGrd <- parseGrid <$> inputTxt

  anim makeGrd 6

  putStrLn $ "part 1: " ++ show (part1 $ makeGrd 3)
  putStrLn $ "part 2: " ++ show (part2 makeGrd)


-- | have a little fun and animate a complete fight
anim :: (AttackPower -> Grid) -> AttackPower -> IO ()
anim mkGrid atkPw = go $ mkGrid atkPw
  where
    go grd = do
      clearScreen
      showGrid grd
      threadDelay (500 * 1000)
      let next = fromJust $ singleRound True grd
      if next == grd then pure () else go next


----------------------------------------------------------------------
-- part 1

-- | iterates the grid with 'singleRound' till ti does not change any more
-- and returns the 'outcomeScore' for that end-state
-- does NOT care if elves die
part1 :: Grid -> Int
part1 = outcomeScore . fromJust . endState True


----------------------------------------------------------------------
-- part 2

-- | tries 'AttackPower's for the elves till they win without anyone dying
-- and returns the 'outcomeScore' for that fight
part2 :: Input -> Int
part2 makeGrd = go 4
  where go attPw =
          maybe (go $ attPw + 1) outcomeScore
          $ endState False (makeGrd attPw)


----------------------------------------------------------------------
-- calculation of the solution-value

-- | outcome-score as defined in the problem: round-nr * sum hp of remaining creatures
outcomeScore :: (Int, Grid) -> Int
outcomeScore (roundNr, endGrid) =
  (roundNr-1) * sum (map (getHitPoints . snd) . creatures $ endGrid)
  where getHitPoints (_, hp, _) = hp


----------------------------------------------------------------------
-- rounds


-- | searches for an end-state for playing the given grid
-- if 'mayElfDie' is false and an elf dies this will return Nothing
-- else it will return the round and the grid at the place where the grid became stationary
-- see 'rounds'
endState :: Bool -> Grid -> Maybe (Int, Grid)
endState mayElfDie grd =
  (\(n,(a,_)) -> (n,a)) <$> (safeHead . dropWhile (\(_,(a,b)) -> a /= b) $ zip [0..] $ zip rnds (tail rnds))
  where
    rnds = rounds mayElfDie grd
    safeHead [] = Nothing
    safeHead (x:_) = Just x


-- | a list of all rounds in a match starting with the given grid
-- if 'mayElfDie' is false then this will stop as soon as a elf dies
-- if 'mayElfDie' is true it will go on forever but the sequence will become stationary at some point
rounds :: Bool -> Grid -> [Grid]
rounds mayElfDie = iterateMaybe (singleRound mayElfDie)


-- | plays a single round using the problems rules
-- will return Nothing when 'mayElfDie' is false and one died this round
-- if not it will always return a new grid but it might be the same as before
-- if no creature moved nor attacked
singleRound :: Bool -> Grid -> Maybe Grid
singleRound mayElfDie grd = foldM unitActions grd $ map fst $ creatures grd
  where
    unitActions grd' coord =
      case M.lookup coord grd' of
        Just (Creature _ _ _) ->
          let (to, grd'') = move coord grd'
          in attack mayElfDie to grd''
        _ -> Just grd'


-- | helper: iterates a function over a starting point till using 'f' till
-- 'f' returnes a 'Nothing'
iterateMaybe :: (a -> Maybe a) -> a -> [a]
iterateMaybe f !x =
  case f x of
    Nothing -> []
    Just x' -> x : iterateMaybe f x'


----------------------------------------------------------------------
-- attacking

-- | tries to attack with the creature on the given 'from' coord
-- gives back the new grid-state after the attack
-- if the 'mayElfDie' parameter is true it will return Nothing
-- IFF some elf died due to this attack
attack :: Bool -> Coord -> Grid -> Maybe Grid
attack mayElfDie from grd =
  case defenders grd attacker from of
    -- at least one defender
    (defHP, at, defAP) : _
      -- defender has enough hitpoints and survives?
      | defHP > ap -> Just $ M.insert at (Creature defender (defHP-ap) defAP) grd
      -- defender is an elf that dies but no elves should die?
      | defender == Elf && not mayElfDie -> Nothing
      -- defender dies and it's ok
      | otherwise  -> Just $ M.insert at Free grd
    -- nothing to attack:
    []             -> Just $ grd
  where
    (Creature attacker _ ap) = grd M.! from
    defender = enemy attacker


-- | get's all enemies that are in reach for the given creature type at the given coord
-- in reach means that the enemy is in the neighbourhood
defenders :: Grid -> Creature -> Coord -> [(HitPoints, Coord, AttackPower)]
defenders grd attacker (r,c) =
  sort . map addStats . filter (isCreature grd defender) $ [ (r-1,c), (r,c-1), (r,c+1), (r+1,c) ]
  where defender = enemy attacker
        addStats coord =
          let (Creature _ hp ap) = grd M.! coord
          in (hp, coord, ap)


----------------------------------------------------------------------
-- movement

-- | moves the creature at the current coord
-- returning the coordinate it moved to and the new grid-state
-- if it did not move the returned coord and grid are 'from' and 'grd'
move :: Coord -> Grid -> (Coord, Grid)
move from grd =
  case findMove grd from of
    Nothing -> (from, grd)
    Just to -> (to, M.insert to c $ M.insert from Free $ grd)
  where c = grd M.! from


----------------------------------------------------------------------
-- path-finding

-- | finds the next move by using A* to find the optimal path (if it exists)
-- and then returning the second step of that (the first one is the current coord)
findMove :: Grid -> Coord -> Maybe Coord
findMove grd from =
  listToMaybe $ drop 1 $ AStar.aStar params from
  where params = astarConfig grd from


-- | path-finding uses my 'Utils.AStar.aStar' algorith
-- this functions sets up the parameters:
--  - no heuristic - don't want this because of the problems setup
--    (for example it can be that the actual distance is the same if a creature moves left or bottom
--    and the manhattan-distance would be less if it goes bottom but the problem says it should go left
--    in the examples there is a situation like this between step 24 and 25)
--  - neighbours are all free cells directly to the top, left, right, bottom
--  - goal-coordinates are those where the current creature type can attack an enemy
--  - all coords are distinct (no equivalence classes)
--  - the cost of moving is always 1
astarConfig :: Grid -> Coord -> AStar.Parameter Coord Coord
astarConfig grd from = AStar.Parameter
  (const 0)
  (freeNeighbours grd)
  (isAttackingPos grd attacker)
  id
  (\_ _ -> 1)
  where (Creature attacker _ _) = grd M.! from


-- | is the given creature at the given coord in an attacking position?
-- meaning is there a enemy in the neighbourhood of that coord
isAttackingPos :: Grid -> Creature -> Coord -> Bool
isAttackingPos grd attacker (r,c) =
  any (isCreature grd defender) [ (r-1,c), (r,c-1), (r,c+1), (r+1,c) ]
  where defender = enemy attacker


-- | returns all free coords around the given
freeNeighbours :: Grid -> Coord -> [Coord]
freeNeighbours grd (r,c) =
  filter (isFree grd) [ (r-1,c), (r,c-1), (r,c+1), (r+1,c) ]


-- | is the given coord free?
isFree :: Grid -> Coord -> Bool
isFree grd coord =
  case M.lookup coord grd of
    Just Free -> True
    _         -> False


----------------------------------------------------------------------
-- helpers

-- | the enemy of an elf is a goblin and vice-versa
enemy :: Creature -> Creature
enemy Goblin = Elf
enemy Elf = Goblin


-- | all coords in the grid
coords :: Grid -> [Coord]
coords = M.keys


-- | returns all creatures in the grid
-- by their coords and stats
-- stats are the type, its hit-poitns and it's attack-power
creatures :: Grid -> [(Coord, (Creature, HitPoints, AttackPower))]
creatures = M.toAscList . M.mapMaybe getCreature
  where getCreature (Creature c hp ap ) = Just (c, hp, ap)
        getCreature _                   = Nothing


-- | checks if there is a creature of given type on the given coords in the grid
isCreature :: Grid -> Creature -> Coord -> Bool
isCreature grd creature coord =
  case M.lookup coord grd of
    Just (Creature c _ _) -> c == creature
    _                     -> False


----------------------------------------------------------------------
-- output

-- | displays a grid on the console
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
            Creature Goblin _ _ -> '@'
            Creature Elf _ _ -> '€'
            Wall -> '#'


----------------------------------------------------------------------
-- IO

inputTxt :: IO String
inputTxt = readFile "./src/Day15/input.txt"


----------------------------------------------------------------------
-- parsing

parseGrid :: String -> Input
parseGrid grd elfAp = M.fromList . concat . zipWith (parseLine elfAp) [0..] . lines $ grd


parseLine :: AttackPower -> Int -> String -> [(Coord, Cell)]
parseLine elfAp y = zipWith (parseCell elfAp y) [0..]


parseCell :: AttackPower -> Int -> Int -> Char -> (Coord, Cell)
parseCell _ y x '#' = ((y,x), Wall)
parseCell _ y x 'G' = ((y,x), Creature Goblin 200 3)
parseCell elfAp y x 'E' = ((y,x), Creature Elf 200 elfAp)
parseCell _ y x '.' = ((y,x), Free) 
parseCell _ _ _ e   = error $ "unexpected cell-character: " ++ show e
