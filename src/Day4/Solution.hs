module Day4.Solution where

import           Data.Char (isDigit)
import           Data.List (sort, nub, maximumBy)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (mapMaybe)
import           Data.Ord (comparing)
import           Data.Time.Calendar (fromGregorian)
import           Data.Time.Clock (UTCTime(..), secondsToDiffTime)
import           Text.Parsec


run :: IO ()
run = do
  putStrLn "DAY 4"
  txt <- inputTxt
  let inp = parseInput txt
  let times = sleepTimes inp

  print times

  putStrLn $ "part 1: " ++ show (part1 inp)
  putStrLn $ "part 2: " ++ show (part2 inp)


part1 :: Input -> Int
part1 inp =
  let times = sleepTimes inp
      bestGuard = sleepsTheMost times
      bestMin = bestMinute bestGuard times
  in bestGuard * bestMin


part2 :: Input -> String
part2 = const "???"


guardIds :: Input -> [GuardId]
guardIds = nub . mapMaybe getGuardId
  where getGuardId (Event _ _ (BeginShift gId)) = Just gId
        getGuardId _ = Nothing


sleepsTheMost :: [Sleeps] -> GuardId
sleepsTheMost =
  fst . maximumBy (comparing snd) . Map.toList . totalSleepTimes


bestMinute :: GuardId -> [Sleeps] -> Int
bestMinute gId =
  fst . maximumBy (comparing snd) . Map.toList . sleepMins gId


totalSleepTimes :: [Sleeps] -> Map GuardId Int
totalSleepTimes =
  foldr (\(Sleeps gId mins _ _) -> Map.insertWith (+) gId mins) Map.empty


sleepMins :: GuardId -> [Sleeps] -> Map Int Int
sleepMins gId =
  foldr (\(Sleeps gId' _ f t) m ->
           if gId' == gId then
             foldr (\minute -> Map.insertWith (+) minute 1) m [f..t-1]
           else
             m) Map.empty


sleepTimes :: Input -> [Sleeps]
sleepTimes = go Nothing Nothing
  where
    go _ _ [] = []
    go _ _ (Event _ _ (BeginShift gId) : rest) = go (Just gId) Nothing rest
    go curGuardId _ (Event _ m FallAsleep : rest) = go curGuardId (Just m) rest 
    go (Just curGuardId) (Just fellAsleepAt) (Event _ m WakeUp : rest) = Sleeps curGuardId (m - fellAsleepAt) fellAsleepAt m : go (Just curGuardId) Nothing rest
    go Nothing _ (Event t _ WakeUp : _) = error $ "don't know who woke up at " ++ show t
    go (Just gId) Nothing (Event t _ WakeUp : _) = error $ "don't know when " ++ show gId ++ " fell at sleep but woke up at " ++ show t


data Sleeps
  = Sleeps GuardId Int Int Int
  deriving Show

----------------------------------------------------------------------
-- File input

inputTxt :: IO String
inputTxt = readFile "./src/Day4/input.txt"


----------------------------------------------------------------------
-- Parsing

type Input = [Event]


type GuardId = Int


data Event =
  Event
  { eventTime :: UTCTime
  , eventMinute :: Int
  , eventAction :: Action
  }
  deriving (Show, Eq, Ord)

data Action
  = BeginShift GuardId
  | FallAsleep
  | WakeUp
  deriving (Show, Ord, Eq)


parseInput :: String -> Input
parseInput = sort . map parseLine . lines


type Parser a = Parsec String () a


testLine = "[1518-05-21 23:58] Guard #2879 begins shift"

parseLine :: String -> Event
parseLine = either (error . show) id . parse eventP "line"


eventP :: Parser Event
eventP = do
  _ <- char '['
  (t,m) <- timeP
  _ <- char ']'
  spaces
  a <- actionP
  return $ Event t (fromIntegral m) a
  where
    actionP = choice [ fallAsleepP, wakeUpP, onGuardP ]
    fallAsleepP = const FallAsleep <$> string "falls asleep"
    wakeUpP = const WakeUp <$> string "wakes up"
    onGuardP = do
      _ <- string "Guard #"
      gId <- intP
      _ <- string " begins shift"
      return $ BeginShift gId
    timeP = do
      y <- intP
      _ <- char '-'
      mo <- intP
      _ <- char '-'
      d <- intP
      _ <- char ' '
      h <- intP :: Parser Integer
      _ <- char ':'
      mi <- intP
      return $ (UTCTime (fromGregorian y mo d) (secondsToDiffTime $ 60 * (mi + 60 * h)), mi)
    intP :: (Read a, Num a) => Parser a
    intP = read <$> many1 (satisfy isDigit)
