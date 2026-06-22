module Algorithm.BFS
  ( bfsSpec,
  )
where

import Algorithm.Common
  ( VertexUpdate (..),
    extractPathResult,
    bfsCandidates,
    runVertexUpdate,
    tryImproveDistance,
  )
import Algorithm.Types (AlgorithmSpec (..))
import Graph.Types
import Graph.VertexContext (VertexContext (..), outNeighbors)
import Pregel.Types

bfsSpec :: AlgorithmSpec
bfsSpec =
  AlgorithmSpec
    { specInitState = initState,
      specBootstrap = bootstrap,
      specVertexUpdate = vertexUpdate,
      specExtractResult = extractPathResult
    }

initState :: NodeId -> RunConfig -> VertexState
initState nodeId cfg
  | nodeId == rcSource cfg =
      initialVertexState {vsDistance = Just 0}
  | otherwise =
      initialVertexState

bootstrap :: RunConfig -> [(NodeId, Message)]
bootstrap cfg =
  [ (to, MsgDistance (rcSource cfg) 0)
    | (to, _) <- neighbors (rcGraph cfg) (rcSource cfg)
  ]

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
