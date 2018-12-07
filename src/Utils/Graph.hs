module Utils.Graph
  ( Graph, Edge
  , toGraph
  , topoSort
  , degree0s
  , removeNode
  ) where

import           Data.List (sortBy)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (listToMaybe)
import           Data.Ord (comparing)
import           Data.Set (Set)
import qualified Data.Set as Set


data Graph node = Graph
  { incomming :: Map node (Set node)
  , outgoing  :: Map node (Set node)
  } deriving Show

type Edge node = (node, node)


-- | creates an @Graph from an list of edges
-- counting incomming edges for each node
toGraph :: Ord node => [Edge node] -> Graph node
toGraph eds =
  let out = Map.fromListWith Set.union $ concat [ [(f, Set.singleton t), (t, Set.empty)] | (f,t) <- eds ]
      inc = Map.fromListWith Set.union $ concat [ [(t, Set.singleton f), (f, Set.empty)] | (f,t) <- eds ]
  in Graph inc out



-- | return the nodes in topological order
topoSort :: Ord node => Graph node -> [node]
topoSort gr = do
      c <- maybe [] pure $ topoFirstNode gr
      let gr' = removeNode c gr
      c : topoSort gr'


-- | removes a node from a graph and updates the
-- incoming edge counters
removeNode :: Ord node => node -> Graph node -> Graph node
removeNode node gr =
  let out' = Map.delete node (outgoing gr)
      inc' = Map.delete node $ Map.map (Set.delete node) (incomming gr)
  in Graph inc' out'


-- | the next node in topological order
topoFirstNode :: Ord node => Graph node -> Maybe node
topoFirstNode = listToMaybe . degree0s


-- | returns all nodes with no incomming edges
-- sorted for alphabetic order of the node-names
degree0s :: Ord node => Graph node -> [node]
degree0s =
  map fst . sortBy (comparing fst) . filter (\ (_, set) -> Set.null set) . Map.toList . incomming

