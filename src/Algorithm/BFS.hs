module Algorithm.BFS
  ( bfsPathSpec,
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
import Algorithm.State (PathState, emptyPathState)
import Algorithm.Types (PathAlgorithmSpec (..))
import Graph.Types (NodeId)
import Graph.VertexContext (VertexContext (..), outNeighbors)
import Pregel.Types

bfsPathSpec :: PathAlgorithmSpec
bfsPathSpec =
  PathAlgorithmSpec
    { psInitState = pathInitState,
      psDefaultState = emptyPathState,
      psBootstrap = pathBootstrap (const True),
      psVertexUpdate = vertexUpdate,
      psExtractResult = extractPathResult,
      psMaxSupersteps = atLeastOneSuperstep
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
