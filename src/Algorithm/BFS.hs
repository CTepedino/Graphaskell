module Algorithm.BFS
  ( bfsPathSpec,
  )
where

import Algorithm.Common
  ( bfsCandidates,
    extractPathResult,
    pathBootstrap,
    pathInitState,
    pathMaxSupersteps,
    runVertexUpdate,
    tryImproveDistance,
  )
import Algorithm.Log (PathLogEntry)
import Algorithm.Messages (DistanceMsg (..))
import Algorithm.Observability (pathObserver)
import Algorithm.State (PathState (..), emptyPathState)
import Algorithm.Types (PathAlgorithmSpec (..))
import Graph.Types (NodeId)
import Graph.VertexContext (VertexContext (..), outNeighbors)
import Pregel.Types

type BfsLog = PathLogEntry DistanceMsg

bfsPathSpec :: PathAlgorithmSpec
bfsPathSpec =
  PathAlgorithmSpec
    { psInitState = pathInitState,
      psDefaultState = emptyPathState,
      psBootstrap = pathBootstrap (const True),
      psVertexUpdate = vertexUpdate,
      psExtractResult = extractPathResult,
      psMaxSupersteps = pathMaxSupersteps
    }

vertexUpdate ::
  VertexContext ->
  PathState ->
  [DistanceMsg] ->
  VertexStepResult PathState DistanceMsg BfsLog
vertexUpdate vtx state messages =
  runVertexUpdate
    vtx
    state
    messages
    (bfsUpdate (vcNodeId vtx))
    emitOutgoing
    pathObserver

bfsUpdate :: NodeId -> [DistanceMsg] -> PathState -> Maybe PathState
bfsUpdate nodeId messages =
  tryImproveDistance nodeId (bfsCandidates messages)

emitOutgoing :: VertexContext -> PathState -> [(NodeId, DistanceMsg)]
emitOutgoing vtx state =
  case psDistance state of
    Nothing -> []
    Just dist ->
      [ (to, DistanceMsg (vcNodeId vtx) dist)
        | to <- outNeighbors vtx
      ]
