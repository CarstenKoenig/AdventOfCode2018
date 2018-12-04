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


type Input = [Event]
type Minute = Int
type GuardId = Int


data Event =
  Event
  { eventTime   :: UTCTime
  , eventMinute :: Minute
  , eventAction :: Action
  }
  deriving (Show, Eq, Ord)


data Action
  = BeginShift GuardId
  | FallAsleep
  | WakeUp
  deriving (Show, Ord, Eq)


data Sleeps
  = Sleeps
    { sleepingGuardId :: GuardId
    , asleepFor :: Minute
    , fallsAsleepAt :: Minute
    , wakesUpAt :: Minute
    }
  deriving Show


----------------------------------------------------------------------
-- main

run :: IO ()
run = do
  putStrLn "DAY 4"
  txt <- inputTxt
  let inp = parseInput txt
  let guards = guardIds inp
  let times = sleepTimes inp

  putStrLn $ "part 1: " ++ show (part1 times)
  putStrLn $ "part 2: " ++ show (part2 guards times)


part1 :: [Sleeps] -> Int
part1 times =
  let bestGuard = sleepsTheMost times
      bestMin = bestMinute bestGuard times
  in bestGuard * bestMin


part2 :: [GuardId] -> [Sleeps] -> Int
part2 guards times =
  let (bestGuard, bestMin) = maximumBy (comparing $ \(gId, m) -> timesSlept times gId m) [ (gId, m) | gId <- guards, m <- [0..59] ]
  in bestGuard * bestMin


----------------------------------------------------------------------
-- algorithm

-- | all different Guard-IDs from the input
guardIds :: Input -> [GuardId]
guardIds = nub . mapMaybe getGuardId
  where getGuardId (Event _ _ (BeginShift gId)) = Just gId
        getGuardId _ = Nothing


-- | which guard spend the most time sleeping
sleepsTheMost :: [Sleeps] -> GuardId
sleepsTheMost =
  fst . maximumBy (comparing snd) . Map.toList . totalSleepTimes


-- | counts the times a certain guard slept during a certain minute
timesSlept :: [Sleeps] -> GuardId -> Minute -> Int
timesSlept times gId minute =
  length $ filter (\(Sleeps gId' _ f t) -> gId == gId' && minute >= f && minute < t) times


-- | looks for minute when the given guard slept the most
bestMinute :: GuardId -> [Sleeps] -> Int
bestMinute gId =
  fst . maximumBy (comparing snd) . Map.toList . sleepMins gId


-- | give a map of guards to their total time asleep (in minutes)
totalSleepTimes :: [Sleeps] -> Map GuardId Int
totalSleepTimes =
  foldr (\(Sleeps gId mins _ _) -> Map.insertWith (+) gId mins) Map.empty


-- | give a map of Minute to days asleep at that minute for a certain guard
sleepMins :: GuardId -> [Sleeps] -> Map Minute Int
sleepMins gId =
  foldr (\(Sleeps gId' _ f t) m ->
           if gId' == gId then
             foldr (\minute -> Map.insertWith (+) minute 1) m [f..t-1]
           else
             m) Map.empty


-- | aggregates the input into a list of records indicating who slept how long and at which times
sleepTimes :: Input -> [Sleeps]
sleepTimes = go Nothing Nothing
  where
    go _ _ [] = []
    go _ _ (Event _ _ (BeginShift gId) : rest) = go (Just gId) Nothing rest
    go curGuardId _ (Event _ m FallAsleep : rest) = go curGuardId (Just m) rest 
    go (Just curGuardId) (Just fellAsleepAt) (Event _ m WakeUp : rest) = Sleeps curGuardId (m - fellAsleepAt) fellAsleepAt m : go (Just curGuardId) Nothing rest
    go Nothing _ (Event t _ WakeUp : _) = error $ "don't know who woke up at " ++ show t
    go (Just gId) Nothing (Event t _ WakeUp : _) = error $ "don't know when " ++ show gId ++ " fell at sleep but woke up at " ++ show t


----------------------------------------------------------------------
-- File input

inputTxt :: IO String
inputTxt = readFile "./src/Day4/input.txt"


----------------------------------------------------------------------
-- Parsing

parseInput :: String -> Input
parseInput = sort . map parseLine . lines


type Parser a = Parsec String () a


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
