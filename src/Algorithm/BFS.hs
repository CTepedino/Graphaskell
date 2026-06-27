module Algorithm.BFS
  ( bfsSpec,
  )
where

import Algorithm.Common
  ( VertexUpdate (..),
    extractPathResult,
    bfsCandidates,
    pathBootstrap,
    pathInitState,
    pathMaxSupersteps,
    runVertexUpdate,
    tryImproveDistance,
  )
import Algorithm.Types (AlgorithmSpec (..))
import Graph.Types (NodeId)
import Graph.VertexContext (VertexContext (..), outNeighbors)
import Pregel.Types

bfsSpec :: AlgorithmSpec
bfsSpec =
  AlgorithmSpec
    { specInitState = pathInitState,
      specBootstrap = pathBootstrap (const True),
      specVertexUpdate = vertexUpdate,
      specExtractResult = extractPathResult,
      specMaxSupersteps = pathMaxSupersteps
    }

vertexUpdate ::
  VertexContext ->
  VertexState ->
  [Message] ->
  VertexStepResult
vertexUpdate vtx state messages =
  runVertexUpdate vtx state messages (bfsUpdate (vcNodeId vtx)) emitOutgoing

bfsUpdate :: NodeId -> [Message] -> VertexState -> VertexUpdate
bfsUpdate nodeId messages = tryImproveDistance nodeId (bfsCandidates messages)

emitOutgoing :: VertexContext -> VertexState -> [(NodeId, Message)]
emitOutgoing vtx state =
  case vsDistance state of
    Nothing -> []
    Just dist ->
      [ (to, MsgDistance (vcNodeId vtx) dist)
        | to <- outNeighbors vtx
      ]
