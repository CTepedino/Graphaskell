module Algorithm.BellmanFord
  ( bellmanFordSpec,
  )
where

import Algorithm.Common (VertexUpdate, extractPathResult, runVertexUpdate, tryImproveDistance)
import Algorithm.Types (AlgorithmSpec (..))
import Data.Maybe (mapMaybe)
import Graph.Types
import Graph.VertexContext
  ( VertexContext (..),
    lookupIncomingWeight,
    weightedOutNeighbors,
  )
import Pregel.Types

bellmanFordSpec :: AlgorithmSpec
bellmanFordSpec =
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
    | (to, Just _) <- neighbors (rcGraph cfg) (rcSource cfg)
  ]

vertexUpdate ::
  VertexContext ->
  VertexState ->
  [Message] ->
  VertexStepResult
vertexUpdate vtx state messages =
  runVertexUpdate vtx state messages (bellmanFordUpdate vtx) emitOutgoing

bellmanFordUpdate :: VertexContext -> [Message] -> VertexState -> VertexUpdate
bellmanFordUpdate vtx messages = tryImproveDistance
    (vcNodeId vtx)
    (mapMaybe (weightedCandidate vtx) messages)

weightedCandidate :: VertexContext -> Message -> Maybe (Int, NodeId)
weightedCandidate _ (MsgLabel _) = Nothing
weightedCandidate _ (MsgRank _) = Nothing
weightedCandidate vtx (MsgDistance from dist) =
  fmap (\weight -> (dist + weight, from)) (lookupIncomingWeight vtx from)

emitOutgoing :: VertexContext -> VertexState -> [(NodeId, Message)]
emitOutgoing vtx state =
  case vsDistance state of
    Nothing -> []
    Just dist ->
      [ (to, MsgDistance (vcNodeId vtx) dist)
        | to <- weightedOutNeighbors vtx
      ]
