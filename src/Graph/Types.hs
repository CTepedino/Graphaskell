module Graph.Types
  ( NodeId (..),
    Distance (..),
    Weight (..),
    Algorithm (..),
    Edge (..),
    ValidGraph,
    GraphError (..),
    GraphEndpoint (..),
    nodeCount,
    graphNodes,
    graphEdges,
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
  { gNodes :: [NodeId],
    gEdges :: [Edge],
    gAdj :: Map NodeId [(NodeId, Maybe Weight)]
  }
  deriving (Eq, Show)

newtype ValidGraph = ValidGraph Graph
  deriving (Eq, Show)

data GraphEndpoint
  = EdgeFrom
  | EdgeTo
  deriving (Eq, Show)

data GraphError
  = GraphBuildInvalidNodeCount Int
  | GraphBuildInvalidNodeId GraphEndpoint NodeId Int
  deriving (Eq, Show)

nodeCount :: ValidGraph -> Int
nodeCount (ValidGraph Graph {gNodes = nodes}) =
  length nodes

graphNodes :: ValidGraph -> [NodeId]
graphNodes (ValidGraph Graph {gNodes = nodes}) =
  nodes

graphEdges :: ValidGraph -> [Edge]
graphEdges (ValidGraph Graph {gEdges = edges}) =
  edges

neighbors :: ValidGraph -> NodeId -> [(NodeId, Maybe Weight)]
neighbors (ValidGraph Graph {gAdj = adj}) nodeId =
  Map.findWithDefault [] nodeId adj

buildGraph :: Int -> [Edge] -> Either GraphError ValidGraph
buildGraph nodeTotal edges = do
  if nodeTotal <= 0
    then Left (GraphBuildInvalidNodeCount nodeTotal)
    else do
      mapM_ (validateEdgeEndpoints nodeTotal) edges
      pure (ValidGraph (mkGraph nodeTotal edges))

isValidNode :: ValidGraph -> NodeId -> Bool
isValidNode graph nodeId =
  let NodeId n = nodeId
   in n >= 0 && n < nodeCount graph

mkGraph :: Int -> [Edge] -> Graph
mkGraph nodeTotal edges =
  Graph
    { gNodes = [NodeId n | n <- [0 .. nodeTotal - 1]],
      gEdges = edges,
      gAdj = foldr insertEdge Map.empty edges
    }
  where
    insertEdge (Edge from to weight) adj =
      Map.insertWith (++) from [(to, weight)] adj

validateEdgeEndpoints :: Int -> Edge -> Either GraphError ()
validateEdgeEndpoints nodeTotal (Edge from to _) = do
  validateNodeId nodeTotal EdgeFrom from
  validateNodeId nodeTotal EdgeTo to

validateNodeId :: Int -> GraphEndpoint -> NodeId -> Either GraphError ()
validateNodeId nodeTotal endpoint (NodeId n)
  | n >= 0 && n < nodeTotal =
      Right ()
  | otherwise =
      Left (GraphBuildInvalidNodeId endpoint (NodeId n) (nodeTotal - 1))
