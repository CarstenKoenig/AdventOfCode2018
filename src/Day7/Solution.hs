module Day7.Solution where

import           Data.Char (ord)
import           Data.List (sortBy, foldl')
import           Data.Maybe (listToMaybe)
import           Data.Ord (comparing)
import           Text.Parsec
import qualified Utils.Counter as C


type Input = [Order]

run :: IO ()
run = do
  putStrLn "DAY 7"
  inp <- inputTxt

  putStrLn $ "part 1: " ++ part1 inp
  putStrLn $ "part 2: " ++ show (part2 inp)


time :: Char -> Int
time c = ord c - ord 'A' + 61


type Workers = C.Counter Char Int


data Part2 = Part2
  { workers :: Workers
  , numWorkers :: Int
  , input   :: Input
  , counters :: C.Counter Char Int
  } deriving Show


initPart2 :: Int -> Input -> Part2
initPart2 n inp =
  let deg = degrees inp
  in Part2 initWorkers n inp deg


part2 :: Input -> Int
part2 = execute 0 . initPart2 5


execute :: Int -> Part2 -> Int
execute tm st
  | not (null finishedJobs) =
    let workers' = cleanFinishedWorkers (workers st)
        counters' = foldl' (\cnt f -> updateIncoming (input st) f cnt) (counters st) finishedJobs
    in execute tm $ st { workers = workers', counters = counters' }
  | allFinished && null pending =
    tm
  | not (null pending) && available > 0 =
    let job = head pending
        workers' = queueWork job (time job) (workers st)
    in execute tm $ st { workers = workers' }
  | otherwise =
    execute (tm+1) $ st { workers = tick (workers st)}
  where
    workingOn = C.keys (workers st)
    pending = filter (not . (`elem` workingOn)) $ degree0s (counters st)
    available = numWorkers st - workersBusy (workers st)
    allFinished = workersBusy (workers st) == 0
    finishedJobs = workDone (workers st)



initWorkers :: Workers
initWorkers = C.empty


workersBusy :: Workers -> Int
workersBusy = C.size


queueWork :: Char -> Int -> Workers -> Workers
queueWork c ts = C.add c ts


cleanFinishedWorkers :: Workers -> Workers
cleanFinishedWorkers ws =
  removeWorkers (workDone ws) ws


workDone :: Workers -> [Char]
workDone =
  map fst . filter (\(_,n) -> n == 0) . C.asc


removeWorkers :: [Char] -> Workers -> Workers
removeWorkers cs ws =
  foldl' (\ws' c -> C.remove c ws') ws cs


tick :: Workers -> Workers
tick wks =
  foldl' (\ws w -> C.decr w ws) wks $ C.keys wks


part1 :: Input -> String
part1 = topoSort


topoSort :: Input -> [Char]
topoSort inp = go $ degrees inp
  where
    go :: C.Counter Char Int -> [Char]
    go degs = do
      c <- maybe [] pure $ degree0 degs
      let degs' = updateIncoming inp c degs
      c : go degs'


updateIncoming :: Input -> Char -> C.Counter Char Int -> C.Counter Char Int
updateIncoming inp from cnt =
  let cnt' = C.remove from cnt
  in foldl' (\ cnt'' f -> C.decr f cnt'') cnt' $ map snd $ filter (\(f',_) -> f' == from) inp


degrees :: Input -> C.Counter Char Int
degrees inp = C.fromList $ concat [ [(f, 0), (t,1)] | (f,t) <- inp]


degree0 :: C.Counter Char Int -> Maybe Char
degree0 =
  listToMaybe . map fst . sortBy (comparing fst) . filter (\ (_,n) -> n == 0) . C.asc


degree0s :: C.Counter Char Int -> [Char]
degree0s =
  map fst . sortBy (comparing fst) . filter (\ (_,n) -> n == 0) . C.asc


----------------------------------------------------------------------
-- IO

inputTxt :: IO Input
inputTxt = map parseLine . lines <$> readFile "./src/Day7/input.txt"


parseLine :: String -> Order
parseLine = either (error . show) id . parse orderP "input.txt"


type Parser a = Parsec String () a


orderP :: Parser Order
orderP = (,) <$> (string "Step " *> anyChar <* string " must be finished before step ") <*> (anyChar <* string " can begin.")


type Order
  = (Char, Char)

