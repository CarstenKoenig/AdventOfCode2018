module Utils.Astar
  ( Parameter (..)
  , Path
  , aStar
  , testExample
  ) where


import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe (fromMaybe)
import           Data.OrdPSQ (OrdPSQ)
import qualified Data.OrdPSQ as PQ
import           Data.Set (Set)
import qualified Data.Set as S


aStar :: (Ord eqNode, Ord node, Show node) => Parameter node eqNode -> node -> Path node
aStar params = astar' . initEnvironment params


data Parameter node eqNode
  = Parameter { heuristic  :: node -> Score
              , neighbours :: node -> [node]
              , isGoal     :: node -> Bool
              , eqClass    :: node -> eqNode
              , score      :: node -> node -> Score
              }


type Score = Int
type Path node = [node]
type PSQ node = OrdPSQ node Score ()


astar' :: (Ord eqNode, Ord node, Show node) => Environment node eqNode -> Path node
astar' env =
  case findPromissingNode env of
    Nothing -> []
    Just candidate
      | isGoalNode env candidate -> reverse $ constructPath env candidate
      | otherwise ->
        let open'   = PQ.delete candidate $ open env
            closed' = S.insert (getClass env candidate) $ closed env
            env'    = env { closed = closed', open = open' }
            neighs  =
              filter (not . (`S.member` closed'). getClass env) $
              getNeighbours env candidate
            env''   = foldr (considerStep candidate) env' neighs
        in astar' env''


data Environment node eqNode
  = Env { closed    :: Set eqNode
        , open      :: PSQ node
        , cameFrom  :: Map node node
        , gScores   :: Map node Score
        , fScores   :: Map node Score
        , parameter :: Parameter node eqNode
        }


initEnvironment :: Ord node => Parameter node eqNode -> node -> Environment node eqNode
initEnvironment p start =
  Env S.empty startOpen M.empty startG startF p
  where
    startOpen = PQ.singleton start 0 ()
    startG = M.insert start 0 M.empty
    startF = M.insert start (heuristic p start) M.empty


considerStep :: (Ord node, Ord eqNode, Show node) => node -> node -> Environment node eqNode -> Environment node eqNode
considerStep current next env =
  let tentativeGScore = gScoreFor env current + score (parameter env) current next
      nextFScore      = tentativeGScore + scoreHeuristic env next
      open'           = PQ.insert next nextFScore () (open env)
      knownGScore     = gScoreFor env next
  in
    if tentativeGScore >= knownGScore
    then env { open = open' }
    else env { open = open'
             , cameFrom = M.insert next current (cameFrom env)
             , gScores  = M.insert next tentativeGScore (gScores env)
             , fScores  = M.insert next nextFScore (fScores env)
             }


isGoalNode :: Environment node eqNode -> node -> Bool
isGoalNode env = isGoal $ parameter env


getClass :: Environment node eqNode -> node -> eqNode
getClass env = eqClass $ parameter env


getNeighbours :: Environment node eqNode -> node -> [node]
getNeighbours env = neighbours $ parameter env


scoreHeuristic :: Environment node eqNode -> node -> Score
scoreHeuristic = heuristic . parameter


findPromissingNode :: Ord node => Environment node eqNode -> Maybe node
findPromissingNode env = node <$> PQ.findMin (open env)
  where node (n, _, _) = n


gScoreFor :: Ord node => Environment node eqNode -> node -> Score
gScoreFor env node =
  fromMaybe maxBound $ node `M.lookup` gScores env


constructPath :: Ord node => Environment node eqNode -> node -> Path node
constructPath env node =
  case M.lookup node (cameFrom env) of
    Nothing   -> [node]
    Just from -> node : constructPath env from


example :: Parameter Int Int
example = Parameter
  (\n -> abs (50-n))
  (\n -> filter (\x -> x `mod` 3 /= 0 && x <= 50) $ [ n + delta | delta <- [1,2,5] ] )
  (== 50)
  id
  (\_ _ -> 1)


exampleResult :: [Int]
exampleResult = [0,5,10,11,16,17,22,23,28,29,34,35,40,41,46,47,49,50]


testExample :: IO ()
testExample = do
  let result = aStar example 0
  if result == exampleResult then
     putStrLn "OK"
  else
    error $ "algorithm failed with " ++ show result
