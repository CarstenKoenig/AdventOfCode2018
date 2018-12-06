module Day4.Solution where

import           Control.Arrow ((&&&))
import           Data.Char (isDigit)
import           Data.List (sort, nub, maximumBy)
import           Data.Maybe (mapMaybe)
import           Data.Ord (comparing)
import           Data.Time.Calendar (fromGregorian)
import           Data.Time.Clock (UTCTime(..), secondsToDiffTime)
import           Text.Parsec
import           Utils.Counter (Counter)
import qualified Utils.Counter as C


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


data Nap
  = Nap
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
  let naps = aggregateNaps inp

  putStrLn $ "part 1: " ++ show (part1 naps)
  putStrLn $ "part 2: " ++ show (part2 guards naps)


part1 :: [Nap] -> Int
part1 naps =
  let bestGuard = laziestGuard naps
      favMinute = guardsFavoriteNapMinute bestGuard naps
  in bestGuard * favMinute


part2 :: [GuardId] -> [Nap] -> Int
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
laziestGuard :: [Nap] -> GuardId
laziestGuard =
  fst . C.maximum . totalNapSpanStats


-- | counts the times a certain guard slept during a certain minute
timesSlept :: [Nap] -> GuardId -> Minute -> Int
timesSlept times gId minute =
  length $ filter (\(Nap gId' _ f t) -> gId == gId' && minute >= f && minute < t) times


-- | looks for minute when the given guard slept the most
guardsFavoriteNapMinute :: GuardId -> [Nap] -> Int
guardsFavoriteNapMinute gId =
  fst . C.maximum . guardMinuteStats gId


-- | give a map of guards to their total time asleep (in minutes)
totalNapSpanStats :: [Nap] -> Counter GuardId Minute
totalNapSpanStats =
  C.fromList . map (sleepingGuardId &&& asleepFor)


-- | give a map of Minute to days asleep at that minute for a certain guard
guardMinuteStats :: GuardId -> [Nap] -> Counter Minute Int
guardMinuteStats gId =
  C.fromList . concatMap guardSleeps
  where guardSleeps (Nap gId' _ f t) =
          if gId' == gId then
             [ (minute, 1) | minute <- [f..t-1] ]
          else
            []


-- | aggregates the input into a list of records indicating who slept how long and at which times
aggregateNaps :: Input -> [Nap]
aggregateNaps = go Nothing Nothing
  where
    go _ _ [] = []
    go _ _ (Event _ _ (BeginShift gId) : rest) = go (Just gId) Nothing rest
    go curGuardId _ (Event _ m FallAsleep : rest) = go curGuardId (Just m) rest 
    go (Just curGuardId) (Just fellAsleepAt) (Event _ m WakeUp : rest) = Nap curGuardId (m - fellAsleepAt) fellAsleepAt m : go (Just curGuardId) Nothing rest
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
