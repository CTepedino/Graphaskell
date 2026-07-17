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
    bfsUpdate
    (emitDistanceMessages outNeighbors)

bfsUpdate :: [DistanceMsg] -> PathState -> Maybe PathState
bfsUpdate messages =
  tryImproveDistance (bfsCandidates messages)
