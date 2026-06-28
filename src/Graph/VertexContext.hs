module Graph.VertexContext
  ( VertexContext (..),
    VertexContexts,
    buildVertexContexts,
    lookupIncomingWeight,
    outNeighbors,
    outDegree,
    weightedOutNeighbors,
  )
where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Graph.Types

data VertexContext = VertexContext
  { vcNodeId :: NodeId,
    vcOutEdges :: [(NodeId, Maybe Weight)],
    vcInWeights :: Map NodeId Weight,
    vcNodeCount :: Int
  }
  deriving (Eq, Show)

type VertexContexts = Map NodeId VertexContext

buildVertexContexts :: Graph -> VertexContexts
buildVertexContexts graph =
  let n = nodeCount graph
   in Map.fromList
        [ ( nodeId,
            VertexContext
              { vcNodeId = nodeId,
                vcOutEdges = neighbors graph nodeId,
                vcInWeights = incomingWeights graph nodeId,
                vcNodeCount = n
              }
          )
        | nodeId <- graphNodes graph
        ]

incomingWeights :: Graph -> NodeId -> Map NodeId Weight
incomingWeights graph nodeId =
  Map.fromList
    [ (edgeFrom edge, weight)
      | edge <- graphEdges graph,
        edgeTo edge == nodeId,
        Just weight <- [edgeWeight edge]
    ]

lookupIncomingWeight :: VertexContext -> NodeId -> Maybe Weight
lookupIncomingWeight vtx from =
  Map.lookup from (vcInWeights vtx)

outNeighbors :: VertexContext -> [NodeId]
outNeighbors vtx = map fst (vcOutEdges vtx)

outDegree :: VertexContext -> Int
outDegree vtx = length (vcOutEdges vtx)

weightedOutNeighbors :: VertexContext -> [NodeId]
weightedOutNeighbors vtx =
  [ to
    | (to, Just _) <- vcOutEdges vtx
  ]
