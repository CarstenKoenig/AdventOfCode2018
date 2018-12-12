module Utils.Graph.ConnectedGroups where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as Map

import Data.IntSet (IntSet, (\\))
import qualified Data.IntSet as Set

import Data.Maybe (fromMaybe)

----------------------------------------------------------------------
-- data and types

type Connection = (Id, Id)

type Id = Int

type Graph = IntMap [Id]

type Nodes = IntSet

type Group = IntSet

----------------------------------------------------------------------
-- graph functions

groups :: Graph -> [Group]
groups g = go (nodes g)
  where
    go ns =
      if Set.null ns
      then []
      else
        let gr = epsClosure g (Set.findMin ns)
        in  gr : go (ns \\ gr)


nodes :: Graph -> Nodes
nodes = Set.fromList . Map.keys


epsClosure :: Graph -> Id -> Nodes
epsClosure graph nodeId = go Set.empty [nodeId]
  where
    go visited [] = visited
    go visited (i:is) =
      if i `Set.member` visited
      then go visited is
      else go (Set.insert i visited) (is ++ connections graph i)


connections :: Graph -> Id -> [Id]
connections g i = fromMaybe [] $ Map.lookup i g


emptyGraph :: Graph
emptyGraph = Map.empty


insertCon :: Connection -> Graph -> Graph
insertCon (from,to) =
  Map.insertWith (++) from [to]
  . Map.insertWith (++) to [from]
