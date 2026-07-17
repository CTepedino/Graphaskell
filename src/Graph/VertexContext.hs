module Graph.VertexContext
  ( VertexContext (..),
    VertexContexts,
    buildVertexContexts,
    allNodes,
    lookupIncomingWeight,
    outNeighbors,
    outDegree,
  )
where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Graph.Types

data VertexContext = VertexContext
  { vcNodeId :: NodeId,
    vcOutEdges :: [(NodeId, Weight)],
    vcInWeights :: Map NodeId Weight,
    vcNodeCount :: Int,
    vcAllNodes :: [NodeId]
  }

type VertexContexts = Map NodeId VertexContext

buildVertexContexts :: ValidGraph -> VertexContexts
buildVertexContexts graph =
  let n = nodeCount graph
      nodes = graphNodes graph
   in Map.fromList
        [ ( nodeId,
            VertexContext
              { vcNodeId = nodeId,
                vcOutEdges = neighbors graph nodeId,
                vcInWeights = incomingWeights graph nodeId,
                vcNodeCount = n,
                vcAllNodes = nodes
              }
          )
        | nodeId <- nodes
        ]

incomingWeights :: ValidGraph -> NodeId -> Map NodeId Weight
incomingWeights graph nodeId =
  Map.fromList
    [ (edgeFrom edge, edgeWeight edge)
      | edge <- graphEdges graph,
        edgeTo edge == nodeId
    ]

lookupIncomingWeight :: VertexContext -> NodeId -> Maybe Weight
lookupIncomingWeight vtx from =
  Map.lookup from (vcInWeights vtx)

outNeighbors :: VertexContext -> [NodeId]
outNeighbors vtx = map fst (vcOutEdges vtx)

outDegree :: VertexContext -> Int
outDegree vtx = length (vcOutEdges vtx)

allNodes :: VertexContext -> [NodeId]
allNodes = vcAllNodes
