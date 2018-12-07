module Day7.Solution where

import           Data.Char (ord)
import           Data.List (sortBy, foldl')
import           Data.Maybe (listToMaybe)
import           Data.Ord (comparing)
import           Text.Parsec
import qualified Utils.Counter as C


type Input = [Edge]


run :: IO ()
run = do
  putStrLn "DAY 7"
  inp <- inputTxt

  putStrLn $ "part 1: " ++ part1 inp
  putStrLn $ "part 2: " ++ show (part2 inp)


----------------------------------------------------------------------
-- part 1

part1 :: [Edge] -> String
part1 = topoSort . toGraph

----------------------------------------------------------------------
-- poor mans Graph

data Graph = Graph
  { incomingCounter :: C.Counter Node Int
  , edges :: [Edge]
  } deriving Show

type Edge = (Node, Node)

type Node = Char


-- | creates an @Graph from an list of edges
-- counting incomming edges for each node
toGraph :: [Edge] -> Graph
toGraph eds =
  let incomming = C.fromList $ concat [ [(f, 0), (t,1)] | (f,t) <- eds]
  in Graph incomming eds


-- | return the nodes in topological order
topoSort :: Graph -> [Node]
topoSort gr = do
      c <- maybe [] pure $ nextNode gr
      let gr' = removeNode c gr
      c : topoSort gr'


-- | removes a node from a graph and updates the
-- incoming edge counters
removeNode :: Char -> Graph -> Graph
removeNode node gr =
  let cnt' = C.remove node $ incomingCounter gr
      cnt'' = foldl' (flip C.decr) cnt' $ map snd $ filter (\(f',_) -> f' == node) (edges gr)
  in gr { incomingCounter = cnt'' }


-- | the next node in topological order
nextNode :: Graph -> Maybe Char
nextNode = listToMaybe . degree0s


-- | returns all nodes with no incomming edges
-- sorted for alphabetic order of the node-names
degree0s :: Graph -> [Node]
degree0s =
  map fst . sortBy (comparing fst) . filter (\ (_,n) -> n == 0) . C.asc . incomingCounter


----------------------------------------------------------------------
-- part 2

part2 :: [Edge] -> Seconds
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
initWorkState :: Int -> [Edge] -> WorkState
initWorkState n = WorkState n initWorkers . toGraph


-- | progresses time till all work is done and returns the seconds taken
execute :: Seconds -> WorkState -> Seconds
execute timeSoFar curState@(WorkState maxWorkers curWorkers curGraph)
  -- all workers sleeping and nothing pending -> we are done
  | allFinished && null pending =
    timeSoFar
  -- some workers finished -> update graph and workers
  | not (null finishedJobs) =
    let workers' = cleanFinishedWorkers curWorkers
        graph'   = foldl' (flip removeNode) curGraph finishedJobs
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
    pending = filter (not . (`elem` workingOn)) $ degree0s curGraph
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


parseLine :: String -> Edge
parseLine = either (error . show) id . parse edgeP "input.txt"


type Parser a = Parsec String () a

edgeP :: Parser Edge
edgeP = (,) <$> (string "Step " *> anyChar <* string " must be finished before step ") <*> (anyChar <* string " can begin.")
