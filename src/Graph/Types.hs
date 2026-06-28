module Graph.Types
  ( NodeId (..),
    Distance (..),
    Weight (..),
    Algorithm (..),
    Edge (..),
    Graph (..),
    nodeCount,
    neighbors,
    buildGraph,
    isValidNode,
    zeroDistance,
    succDistance,
    distancePlusWeight,
  )
where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

newtype NodeId = NodeId {unNodeId :: Int}
  deriving (Eq, Ord, Read)

newtype Distance = Distance {unDistance :: Int}
  deriving (Eq, Ord, Read)

newtype Weight = Weight {unWeight :: Int}
  deriving (Eq, Ord, Read)

instance Show NodeId where
  show = show . unNodeId

instance Show Distance where
  show = show . unDistance

instance Show Weight where
  show = show . unWeight

zeroDistance :: Distance
zeroDistance = Distance 0

succDistance :: Distance -> Distance
succDistance (Distance d) = Distance (d + 1)

distancePlusWeight :: Distance -> Weight -> Distance
distancePlusWeight (Distance d) (Weight w) = Distance (d + w)

data Algorithm
  = BFS
  | BellmanFord
  | PageRank
  | ConnectedComponents
  | LabelPropagation
  deriving (Eq, Show, Read)

data Edge = Edge
  { edgeFrom :: NodeId,
    edgeTo :: NodeId,
    edgeWeight :: Maybe Weight
  }
  deriving (Eq, Show)

data Graph = Graph
  { graphNodes :: [NodeId],
    graphEdges :: [Edge],
    graphAdj :: Map NodeId [(NodeId, Maybe Weight)]
  }
  deriving (Eq, Show)

nodeCount :: Graph -> Int
nodeCount = length . graphNodes

neighbors :: Graph -> NodeId -> [(NodeId, Maybe Weight)]
neighbors graph nodeId =
  Map.findWithDefault [] nodeId (graphAdj graph)

buildGraph :: Int -> [Edge] -> Graph
buildGraph nodeTotal edges =
  Graph
    { graphNodes = [NodeId n | n <- [0 .. nodeTotal - 1]],
      graphEdges = edges,
      graphAdj = foldr insertEdge Map.empty edges
    }
  where
    insertEdge (Edge from to weight) adj =
      Map.insertWith (++) from [(to, weight)] adj

isValidNode :: Graph -> NodeId -> Bool
isValidNode graph (NodeId n) =
  n >= 0 && n < nodeCount graph
