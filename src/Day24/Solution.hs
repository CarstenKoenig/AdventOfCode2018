{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
module Day24.Solution where

import Data.Char (isDigit)
import Data.List (groupBy, sortBy)
import Data.Ord (comparing)
import Text.Parsec
import Data.Function (on)


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
  } deriving Show


run :: IO ()
run = do
  putStrLn "DAY 24"


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
