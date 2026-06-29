module Algorithm.BFS
  ( bfsSpec,
  )
where

import Algorithm.Common
  ( atLeastOneSuperstep,
    bfsCandidates,
    emitDistanceMessages,
    extractPathResult,
    pathBootstrap,
    pathInitState,
    runVertexUpdate,
    tryImproveDistance,
  )
import Algorithm.Messages (DistanceMsg)
import Algorithm.Observability (pathObserver)
import Algorithm.State (PathState, emptyPathState)
import Algorithm.Types (AlgorithmSpec (..), PathLog)
import Graph.Types (NodeId)
import Graph.VertexContext (VertexContext (..), outNeighbors)
import Pregel.Types

bfsSpec :: AlgorithmSpec PathState DistanceMsg PathLog
bfsSpec =
  AlgorithmSpec
    { specInitState = pathInitState,
      specDefaultState = emptyPathState,
      specBootstrap = pathBootstrap,
      specVertexUpdate = vertexUpdate,
      specExtractResult = extractPathResult,
      specMaxSupersteps = atLeastOneSuperstep,
      specObserveStep = pathObserver
    }

vertexUpdate ::
  VertexContext ->
  PathState ->
  [DistanceMsg] ->
  VertexStepResult PathState DistanceMsg
vertexUpdate vtx state messages =
  runVertexUpdate
    vtx
    state
    messages
    (bfsUpdate (vcNodeId vtx))
    (emitDistanceMessages outNeighbors)

bfsUpdate :: NodeId -> [DistanceMsg] -> PathState -> Maybe PathState
bfsUpdate nodeId messages =
  tryImproveDistance nodeId (bfsCandidates messages)
