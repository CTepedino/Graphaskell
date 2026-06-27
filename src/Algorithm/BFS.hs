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
import Algorithm.Log (PathLogEntry)
import Algorithm.Messages (DistanceMsg (..))
import Algorithm.State (PathState (..), emptyPathState)
import Algorithm.Types (AlgorithmSpec (..))
import Graph.Types (NodeId)
import Graph.VertexContext (VertexContext (..), outNeighbors)
import Pregel.Types

type BfsLog = PathLogEntry DistanceMsg

bfsSpec :: AlgorithmSpec PathState DistanceMsg BfsLog
bfsSpec =
  AlgorithmSpec
    { specInitState = pathInitState,
      specDefaultState = emptyPathState,
      specBootstrap = pathBootstrap (const True),
      specVertexUpdate = vertexUpdate,
      specExtractResult = extractPathResult,
      specMaxSupersteps = pathMaxSupersteps
    }

vertexUpdate ::
  VertexContext ->
  PathState ->
  [DistanceMsg] ->
  VertexStepResult PathState DistanceMsg BfsLog
vertexUpdate vtx state messages =
  runVertexUpdate vtx state messages (bfsUpdate (vcNodeId vtx)) emitOutgoing

bfsUpdate :: NodeId -> [DistanceMsg] -> PathState -> VertexUpdate PathState DistanceMsg BfsLog
bfsUpdate nodeId messages = tryImproveDistance nodeId (bfsCandidates messages)

emitOutgoing :: VertexContext -> PathState -> [(NodeId, DistanceMsg)]
emitOutgoing vtx state =
  case psDistance state of
    Nothing -> []
    Just dist ->
      [ (to, DistanceMsg (vcNodeId vtx) dist)
        | to <- outNeighbors vtx
      ]
