{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
module Day24.Solution where

import           Control.Arrow ((&&&))
import           Data.Char (isDigit)
import           Data.Function (on)
import           Data.List (groupBy, sortBy, maximumBy, foldl')
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Ord (Down(..), comparing)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Text.Parsec hiding (State)


type HitPoints = Int
type AttackDamage = Int
type Units = Int
type Initiative = Int

data AttackType = Fire | Cold | Slashing | Radiation | Bludgeoning
  deriving (Show, Eq, Ord, Enum, Bounded)

type Army = [Group]

data Group = Group
  { groupUnits    :: Units
  , unitHitpoints :: HitPoints
  , attackDamage  :: Int
  , attackType    :: AttackType
  , weaknesses    :: [AttackType]
  , immunites     :: [AttackType]
  , initiative    :: Initiative
  } deriving (Eq, Show)


type GroupNr = Int

data State = State
  { groups        :: Map GroupNr Group
  , immuneArmy    :: Set GroupNr
  , infectionArmy :: Set GroupNr
  } deriving (Eq, Show)


part1 :: Input -> Int
part1 inp =
  let state = initState inp
      end = simulate state
  in unitsLeft end


part2 :: Input -> Int
part2 inp =
  let start = initState inp
      sims = map simulate [ boost by start | by <- [1..] ]
      won = dropWhile (not . infectionLost) sims
  in unitsLeft $ head $ won


unitsLeft :: State -> Int
unitsLeft = sum . map groupUnits . Map.elems . groups


infectionLost :: State -> Bool
infectionLost = Set.null . infectionArmy


immuneLost :: State -> Bool
immuneLost = Set.null . immuneArmy


boost :: AttackDamage -> State -> State
boost boostBy state = foldl' boostGroup state $ immuneArmy state
  where
    boostGroup st grpNr =
      let gr = getGroup st grpNr
          newGr = gr { attackDamage = attackDamage gr + boostBy }
      in st { groups = Map.insert grpNr newGr $ groups st }


initState :: (Army, Army) -> State
initState (imms, infs) =
  let immNrs = Set.fromList $ take (length imms) [1..]
      infNrs = Set.fromList $ take (length infs) [length imms+1..]
      grps   = Map.fromList $ zip [1..] (imms ++ infs)
  in State grps immNrs infNrs


simulate :: State -> State
simulate state =
  let state' = oneRound state
  in if state == state' then state else simulate state'


oneRound :: State -> State
oneRound state =
  let sels = selectionPhase state
      afterAtk = attackPhase sels state
  in removeDead afterAtk


removeDead :: State -> State
removeDead state =
  let groups' = Map.filter (\g -> groupUnits g > 0) $ groups state
      immArmy = Set.filter (\nr -> nr `Map.member` groups') $ immuneArmy state
      infArmy = Set.filter (\nr -> nr `Map.member` groups') $ infectionArmy state
  in State groups' immArmy infArmy


attackPhase :: [(GroupNr, Maybe GroupNr)] -> State -> State
attackPhase sels state =
  foldl' (flip attack) state sels


attack :: (GroupNr, Maybe GroupNr) -> State -> State
attack (_, Nothing) state = state
attack (attNr, Just defNr) state =
  let att = getGroup state attNr
      def = getGroup state defNr
      dmg = calcDamage att def
      killed = dmg `div` unitHitpoints def
  in state { groups = Map.insert defNr (def { groupUnits = max 0 (groupUnits def - killed)}) (groups state) }


getGroup :: State -> GroupNr -> Group
getGroup state = (groups state Map.!)


selectionPhase :: State -> [(GroupNr, Maybe GroupNr)]
selectionPhase state =
  let selections =
        targetSelection state (immuneArmy state) (infectionArmy state)
        ++ targetSelection state (infectionArmy state) (immuneArmy state)
  in sortBy (comparing (negate . initiative . getGroup state . fst)) selections


targetSelection :: State -> Set GroupNr -> Set GroupNr -> [(GroupNr, Maybe GroupNr)]
targetSelection state attackers =
  go (sortBy (comparing (Down . order . snd)) $ map (id &&& getGroup state) $ Set.toList attackers)
  where
    go :: [(GroupNr, Group)] -> Set GroupNr -> [(GroupNr, Maybe GroupNr)]
    go [] _ = []
    go ((attNr, att):rest) defs
      | Set.null defs = []
      | otherwise =
        let (d,remDefs) = highestDmgPotential state att defs
        in (attNr, d) : go rest remDefs
    order gr = (effectivePower gr, initiative gr)


highestDmgPotential :: State -> Group -> Set GroupNr -> (Maybe GroupNr, Set GroupNr)
highestDmgPotential state att defNrs =
  let target = maximumBy (comparing (order . getGroup state)) $ Set.toList defNrs
  in
    if calcDamage att (getGroup state target) <= 0
    then (Nothing, defNrs)
    else (Just target, Set.delete target defNrs)
  where
    order gr = (calcDamage att gr, effectivePower gr, initiative gr)


calcDamage :: Group -> Group -> HitPoints
calcDamage att def =
  let factor = calcModifier (attackType att)
  in effectivePower att * factor
  where
    calcModifier attType =
      if attType `elem` immunites def
      then 0
      else if attType `elem` weaknesses def
      then 2
      else 1


effectivePower :: Group -> AttackDamage
effectivePower gr = groupUnits gr * attackDamage gr


run :: IO ()
run = do
  putStrLn "DAY 24"

  inp <- inputTxt
  putStrLn $ "part 1: " ++ show (part1 inp)
  putStrLn $ "part 2: " ++ show (part2 inp)

exampleTxt :: IO Input
exampleTxt = parseInput <$> readFile "./src/Day24/example.txt"


inputTxt :: IO Input
inputTxt = parseInput <$> readFile "./src/Day24/input.txt"


type Input = (Army, Army)

parseInput :: String -> Input
parseInput = either (error . show) id . parse inputP "input.txt"


type Parser a = Parsec String () a


inputP :: Parser Input
inputP = do
  (_, imm) <- armyP
  _ <- many newline
  (_, inf) <- armyP
  pure (imm, inf)


armyP :: Parser (String, Army)
armyP = do
  name <- many1 (noneOf ":\n\r") <* char ':' <* newline
  army <- many (groupP <* newline)
  pure (name, army)


groupP :: Parser Group
groupP = do
  (us, hp)       <- unitsP
  (im, wk)       <- weaknessesAndImmunitiesP
  (attPw, attTy) <- attackP
  ini            <- initiativeP
  return $ Group us hp attPw attTy wk im ini


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
      parts <- imuOrWkP `sepBy1` (string "; ")
      case collect parts of
        [(False, wks), (True, ims)] -> pure (concat wks, concat ims)
        [(False, wks)]              -> pure (concat wks, [])
        [(True, ims)]               -> pure ([], concat ims)
        []                          -> pure ([], [])
    imuOrWkP = choice [ wksP, imusP ]
    wksP = string "weak to " *> ((True, ) <$> (attackTypeP `sepBy1` string ", "))
    imusP = string "immune to " *> ((False, ) <$> (attackTypeP `sepBy1` string ", "))
    collect = map pullOut . groupBy ((==) `on` fst) . sortBy (comparing fst)
    pullOut xs@((a,_):_) = (a, map snd xs)


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

intP :: Parser Int
intP = choice [ negate <$> (char '-' *> numP), numP ]


numP :: Parser Int
numP = read <$> many1 (satisfy isDigit)
