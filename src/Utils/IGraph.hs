module Utils.IGraph
  ( Graph, Edge
  , toGraph
  , topoSort
  , degree0s
  , removeNode
  ) where

import           Data.Bifunctor (bimap)
import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IMap
import           Data.IntSet (IntSet)
import qualified Data.IntSet as ISet
import           Data.List (sortBy)
import           Data.Maybe (listToMaybe)
import           Data.Ord (comparing)


data Graph node = Graph
  { incomming :: IntMap IntSet
  , outgoing  :: IntMap IntSet
  } deriving Show

type Edge node = (node, node)


-- | creates an @Graph from an list of edges
-- counting incomming edges for each node
toGraph :: Enum node => Ord node => [Edge node] -> Graph node
toGraph eds =
  let
    keyEds = bimap fromEnum fromEnum <$> eds
    out = IMap.fromListWith ISet.union $ concat [ [(f, ISet.singleton t), (t, ISet.empty)] | (f,t) <- keyEds ]
    inc = IMap.fromListWith ISet.union $ concat [ [(t, ISet.singleton f), (f, ISet.empty)] | (f,t) <- keyEds ]
  in Graph inc out


-- | return the nodes in topological order
topoSort :: Enum node => Ord node => Graph node -> [node]
topoSort = map toEnum . go
  where
    go gr = do
      key <- maybe [] pure $ topoFirstNode gr
      let gr' = removeNode' key gr
      key : go gr'


-- | removes a node from a graph and updates the
-- incoming edge counters
removeNode :: Enum node => Ord node => node -> Graph node -> Graph node
removeNode node = removeNode' (fromEnum node)


-- | removes a node from a graph and updates the
-- incoming edge counters
removeNode' :: Int -> Graph node -> Graph node
removeNode' nodeKey gr =
  let out' = IMap.delete nodeKey (outgoing gr)
      inc' = IMap.delete nodeKey $ IMap.map (ISet.delete $ nodeKey) (incomming gr)
  in Graph inc' out'


-- | the next node in topological order
topoFirstNode :: Ord node => Graph node -> Maybe Int
topoFirstNode = listToMaybe . degree0s'


-- | returns all nodes with no incomming edges
-- sorted for alphabetic order of the node-names
degree0s :: Enum node => Ord node => Graph node -> [node]
degree0s = map toEnum . degree0s'


degree0s' :: Ord node => Graph node -> [Int]
degree0s' =
  map fst . sortBy (comparing fst) . filter (\ (_, set) -> ISet.null set) . IMap.toList . incomming
