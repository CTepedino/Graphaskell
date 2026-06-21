module Graph.Types
  ( NodeId,
    Algorithm (..),
    Edge (..),
    Graph (..),
    nodeCount,
    neighbors,
    buildGraph,
    isValidNode,
  )
where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

type NodeId = Int

data Algorithm = BFS | DFS | Dijkstra
  deriving (Eq, Show, Read)

data Edge = Edge
  { edgeFrom :: NodeId,
    edgeTo :: NodeId,
    edgeWeight :: Maybe Int
  }
  deriving (Eq, Show)

data Graph = Graph
  { graphNodes :: [NodeId],
    graphEdges :: [Edge],
    graphAdj :: Map NodeId [(NodeId, Maybe Int)]
  }
  deriving (Eq, Show)

nodeCount :: Graph -> Int
nodeCount = length . graphNodes

neighbors :: Graph -> NodeId -> [(NodeId, Maybe Int)]
neighbors graph nodeId =
  Map.findWithDefault [] nodeId (graphAdj graph)

buildGraph :: Int -> [Edge] -> Graph
buildGraph nodeTotal edges =
  Graph
    { graphNodes = [0 .. nodeTotal - 1],
      graphEdges = edges,
      graphAdj = foldr insertEdge Map.empty edges
    }
  where
    insertEdge (Edge from to weight) adj =
      Map.insertWith (++) from [(to, weight)] adj

isValidNode :: Graph -> NodeId -> Bool
isValidNode graph nodeId =
  nodeId >= 0 && nodeId < nodeCount graph
