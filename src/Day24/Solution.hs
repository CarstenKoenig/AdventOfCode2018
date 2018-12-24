{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
module Day24.Solution
  ( run
  ) where

import           Control.Arrow ((&&&))
import           Data.Char (isDigit)
import           Data.Function (on)
import           Data.List (groupBy, maximumBy, foldl', sortOn)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Ord (Down(..), comparing)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Text.Parsec hiding (State)


----------------------------------------------------------------------
-- main

run :: IO ()
run = do
  putStrLn "DAY 24"

  inp <- inputTxt
  putStrLn $ "part 1: " ++ show (part1 inp)
  putStrLn $ "part 2: " ++ show (part2 inp)


-- | runs 'simulate' for the problems input
-- and counts the units left
part1 :: Input -> Int
part1 = unitsLeft . simulate . initState


-- | incrementally boosts the immunesystem
-- till 'simulate' results with no infection left
-- and counts the remaining units in the first such case
part2 :: Input -> Int
part2 inp =
  let start = initState inp
      sims  = map simulate [ boost by start | by <- [1..] ]
      won   = dropWhile (not . infectionLost) sims
  in unitsLeft $ head won


----------------------------------------------------------------------
-- State managment

-- | an army is a Set of Group-Numbers

-- | state of a running simulation
data State = State
  { groups        :: Map GroupNr Group
  , immuneArmy    :: Army
  , infectionArmy :: Army
  } deriving (Eq, Show)

type GroupNr = Int
type Army = Set GroupNr


-- | initializes a starting 'State' from the problem 'Input'
initState :: Input -> State
initState (imms, infs) =
  let immNrs = Set.fromList $ take (length imms) [1..]
      infNrs = Set.fromList $ take (length infs) [length imms+1..]
      grps   = Map.fromList $ zip [1..] (imms ++ infs)
  in State grps immNrs infNrs


-- | increases every immunesystem-groups attack-power by the given amount
boost :: AttackDamage -> State -> State
boost boostBy state = foldl' boostGroup state $ immuneArmy state
  where
    boostGroup st grpNr =
      let gr    = getGroup st grpNr
          newGr = gr { attackDamage = attackDamage gr + boostBy }
      in st { groups = Map.insert grpNr newGr $ groups st }


-- | counts how many units are left - including both armies
unitsLeft :: State -> Int
unitsLeft = sum . map groupUnits . Map.elems . groups


-- | True if the infection-army has no groups left
infectionLost :: State -> Bool
infectionLost = Set.null . infectionArmy


-- | returns a group by it's number
getGroup :: State -> GroupNr -> Group
getGroup state = (groups state Map.!)


----------------------------------------------------------------------
-- simulation

-- | runs 'oneRound' till the state doesn't change any more
simulate :: State -> State
simulate state =
  let state' = oneRound state
  in if state == state' then state else simulate state'


-- | one round:
-- selection phase > attacking phase > remove dead groups
oneRound :: State -> State
oneRound state =
  let sels     = selectionPhase state
      afterAtk = attackPhase sels state
  in removeDead afterAtk


-- | runs 'attack' with a selection as returned by 'selectionPhase'
attackPhase :: [(GroupNr, Maybe GroupNr)] -> State -> State
attackPhase sels state =
  foldl' (flip attack) state sels


-- | let a group fight if it did select a defender
-- see problem description for details
attack :: (GroupNr, Maybe GroupNr) -> State -> State
attack (_, Nothing) state = state
attack (attNr, Just defNr) state =
  let att    = getGroup state attNr
      def    = getGroup state defNr
      dmg    = calcDamage att def
      killed = dmg `div` unitHitpoints def
  in state { groups = Map.insert defNr (def { groupUnits = max 0 (groupUnits def - killed)}) (groups state) }


-- | removes all dead groups - from both the Map and the Sets
removeDead :: State -> State
removeDead state =
  let groups' = Map.filter (\g -> groupUnits g > 0) $ groups state
      immArmy = Set.filter (`Map.member` groups') $ immuneArmy state
      infArmy = Set.filter (`Map.member` groups') $ infectionArmy state
  in State groups' immArmy infArmy


-- | selection phase - see problem description for details
-- let's each group select it's defender (if any) and sorts by initiative
selectionPhase :: State -> [(GroupNr, Maybe GroupNr)]
selectionPhase state =
  let selections =
        targetSelection state (immuneArmy state) (infectionArmy state)
        ++ targetSelection state (infectionArmy state) (immuneArmy state)
  in sortOn (Down . initiative . getGroup state . fst) selections


-- | target selection for an army agains another
targetSelection :: State -> Set GroupNr -> Set GroupNr -> [(GroupNr, Maybe GroupNr)]
targetSelection state attackers =
  go (sortOn (Down . order . snd) $ map (id &&& getGroup state) $ Set.toList attackers)
  where
    go [] _ = []
    go ((attNr, att):rest) defs
      | Set.null defs = []
      | otherwise =
        let (d,remDefs) = highestDmgPotential state att defs
        in (attNr, d) : go rest remDefs
    order = effectivePower &&& initiative


-- | calculates a selection for a attack group by the rules from the problem description
highestDmgPotential :: State -> Group -> Set GroupNr -> (Maybe GroupNr, Set GroupNr)
highestDmgPotential state att defNrs =
  let target = maximumBy (comparing (order . getGroup state)) $ Set.toList defNrs
  in
    if calcDamage att (getGroup state target) <= 0
    then (Nothing, defNrs)
    else (Just target, Set.delete target defNrs)
  where
    order = calcDamage att &&& effectivePower &&& initiative

----------------------------------------------------------------------
-- group management


data Group = Group
  { groupUnits    :: Units
  , unitHitpoints :: HitPoints
  , attackDamage  :: AttackDamage
  , attackType    :: AttackType
  , weaknesses    :: [AttackType]
  , immunites     :: [AttackType]
  , initiative    :: Initiative
  } deriving (Eq, Show)


type HitPoints = Int

type AttackDamage = Int

type Units = Int

type Initiative = Int

data AttackType = Fire | Cold | Slashing | Radiation | Bludgeoning
  deriving (Show, Eq, Ord, Enum, Bounded)


-- | calculates the hitpoint-damage a attacking group would cause by a defending
calcDamage :: Group -> Group -> HitPoints
calcDamage att def =
  effectivePower att * calcModifier (attackType att)
  where
    calcModifier attType
      | attType `elem` immunites def  = 0
      | attType `elem` weaknesses def = 2
      | otherwise                     = 1


-- | the effective power of a group
effectivePower :: Group -> AttackDamage
effectivePower gr = groupUnits gr * attackDamage gr


----------------------------------------------------------------------
-- IO

type Input = ([Group], [Group])


inputTxt :: IO Input
inputTxt = parseInput <$> readFile "./src/Day24/input.txt"


----------------------------------------------------------------------
-- parsing

parseInput :: String -> Input
parseInput = either (error . show) id . parse inputP "input.txt"


type Parser a = Parsec String () a


inputP :: Parser Input
inputP = do
  (_, imm) <- armyP
  _ <- many newline
  (_, inf) <- armyP
  pure (imm, inf)


armyP :: Parser (String, [Group])
armyP = do
  name <- many1 (noneOf ":\n\r") <* char ':' <* newline
  army <- many (groupP <* newline)
  pure (name, army)


groupP :: Parser Group
groupP = do
  (us, hp)       <- unitsP
  (im, wk)       <- weaknessesAndImmunitiesP
  (attPw, attTy) <- attackP
  Group us hp attPw attTy wk im <$> initiativeP


unitsP :: Parser (Units, HitPoints)
unitsP = (,) <$> unsP <*> hitpointP
  where
    unsP = numP <* string " units each with "
    hitpointP = numP <* string " hit points "

attackP :: Parser (AttackDamage, AttackType)
attackP = (,) <$> (string "with an attack that does " *> numP <* spaces) <*> (attackTypeP <* string " damage ")


initiativeP :: Parser Initiative
initiativeP = string "at initiative " *> numP


weaknessesAndImmunitiesP :: Parser ([AttackType],[AttackType])
weaknessesAndImmunitiesP = (between (char '(') (char ')') imuWksP <|> pure ([],[])) <* spaces
  where
    imuWksP :: Parser ([AttackType], [AttackType])
    imuWksP = do
      parts <- imuOrWkP `sepBy1` string "; "
      case collect parts of
        [(False, wks), (True, ims)] -> pure (concat wks, concat ims)
        [(False, wks)]              -> pure (concat wks, [])
        [(True, ims)]               -> pure ([], concat ims)
        []                          -> pure ([], [])
        _                           -> error "should never happen"
    imuOrWkP = choice [ wksP, imusP ]

    wksP = string "weak to " *> ((True, ) <$> (attackTypeP `sepBy1` string ", "))

    imusP = string "immune to " *> ((False, ) <$> (attackTypeP `sepBy1` string ", "))

    collect = map pullOut . groupBy ((==) `on` fst) . sortOn fst

    pullOut xs@((a,_):_) = (a, map snd xs)
    pullOut [] = error "empty list"


attackTypeP :: Parser AttackType
attackTypeP = choice (map mkP types)
  where
    types = [ ("slashing", Slashing)
            , ("radiation", Radiation)
            , ("fire", Fire)
            , ("bludgeoning", Bludgeoning)
            , ("cold", Cold)
            ]
    mkP (s,t) = string s *> pure t

numP :: Parser Int
numP = read <$> many1 (satisfy isDigit)
