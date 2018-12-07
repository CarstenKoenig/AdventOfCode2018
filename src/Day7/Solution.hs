module Day7.Solution where

import           Data.Char (ord)
import           Data.List (foldl')
import           Text.Parsec
import qualified Utils.Counter as C
import qualified Utils.Graph as G


type Input = [Edge]

type Graph = G.Graph Node

type Edge = G.Edge Node

type Node = Char

run :: IO ()
run = do
  putStrLn "DAY 7"
  inputGraph <- G.toGraph <$> inputTxt

  putStrLn $ "part 1: " ++ part1 inputGraph
  putStrLn $ "part 2: " ++ show (part2 inputGraph)


----------------------------------------------------------------------
-- part 1

-- | part 1 is just a topological sort of the input graph
part1 :: Graph -> String
part1 = G.topoSort


----------------------------------------------------------------------
-- part 2

part2 :: Graph -> Seconds
part2 = execute 0 . initWorkState 5


-- | time it takes to finish node
time :: Node -> Seconds
time node = ord node - ord 'A' + 61


----------------------------------------------------------------------
-- WorkState

-- | keeps track of the workers and the graph of nodes to work on
data WorkState = WorkState
  { numWorkers :: Int
  , workers :: Workers
  , graph :: Graph
  } deriving Show


-- | inits the state for part 2
initWorkState :: Int -> Graph -> WorkState
initWorkState n = WorkState n initWorkers


-- | progresses time till all work is done and returns the seconds taken
execute :: Seconds -> WorkState -> Seconds
execute timeSoFar curState@(WorkState maxWorkers curWorkers curGraph)
  -- all workers sleeping and nothing pending -> we are done
  | allFinished && null pending =
    timeSoFar
  -- some workers finished -> update graph and workers
  | not (null finishedJobs) =
    let workers' = cleanFinishedWorkers curWorkers
        graph'   = foldl' (flip G.removeNode) curGraph finishedJobs
    in execute timeSoFar $ curState { workers = workers', graph = graph' }
  -- work pending and workers available -> give them some work!
  | not (null pending) && available > 0 =
    let workers' = foldl' (flip queueWork) curWorkers (take available pending)
    in execute timeSoFar $ curState { workers = workers' }
  -- nothing pending and some workers busy -> progress time
  | otherwise =
    execute (timeSoFar+1) $ curState { workers = tick curWorkers }
  where
    workingOn = C.keys curWorkers
    pending = filter (not . (`elem` workingOn)) $ G.degree0s curGraph
    available = maxWorkers - workersBusy curWorkers
    allFinished = workersBusy curWorkers == 0
    finishedJobs = workDone curWorkers


----------------------------------------------------------------------
-- Worker structure

-- | workers are just counters of nodes in work and the time left to finish them
type Workers =
  C.Counter Node Seconds

type Seconds = Int


-- | we start with no workers doing anything
initWorkers :: Workers
initWorkers = C.empty


-- | how many workers are busy?
workersBusy :: Workers -> Int
workersBusy = C.size


-- | add a job working on a Node
queueWork :: Node -> Workers -> Workers
queueWork node = C.add node (time node)


-- | removes all workers finished with their job
cleanFinishedWorkers :: Workers -> Workers
cleanFinishedWorkers ws =
  removeWorkers (workDone ws) ws


-- | lists all workers finished (no time left)
workDone :: Workers -> [Node]
workDone =
  map fst . filter (\(_,n) -> n == 0) . C.asc


-- | removes workers
removeWorkers :: [Node] -> Workers -> Workers
removeWorkers nodes ws =
  foldl' (flip C.remove) ws nodes


-- | let the workers progress one second
tick :: Workers -> Workers
tick wks =
  foldl' (flip C.decr) wks $ C.keys wks


----------------------------------------------------------------------
-- IO

inputTxt :: IO [Edge]
inputTxt = map parseLine . lines <$> readFile "./src/Day7/input.txt"

inputTst :: IO [Edge]
inputTst = map parseLine . lines <$> readFile "./src/Day7/test.txt"

parseLine :: String -> Edge
parseLine = either (error . show) id . parse edgeP "input.txt"


type Parser a = Parsec String () a

edgeP :: Parser Edge
edgeP = (,) <$> (string "Step " *> anyChar <* string " must be finished before step ") <*> (anyChar <* string " can begin.")
