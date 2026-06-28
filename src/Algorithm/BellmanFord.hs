module Algorithm.BellmanFord
  ( bellmanFordPathSpec,
  )
where

import Algorithm.Common
  ( extractPathResult,
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
import Data.Maybe (isJust, mapMaybe)
import Graph.Types (NodeId)
import Graph.VertexContext
  ( VertexContext (..),
    lookupIncomingWeight,
    weightedOutNeighbors,
  )
import Pregel.Types

type BellmanFordLog = PathLogEntry DistanceMsg

bellmanFordPathSpec :: PathAlgorithmSpec
bellmanFordPathSpec =
  PathAlgorithmSpec
    { psInitState = pathInitState,
      psDefaultState = emptyPathState,
      psBootstrap = pathBootstrap isJust,
      psVertexUpdate = vertexUpdate,
      psExtractResult = extractPathResult,
      psMaxSupersteps = pathMaxSupersteps
    }

vertexUpdate ::
  VertexContext ->
  PathState ->
  [DistanceMsg] ->
  VertexStepResult PathState DistanceMsg BellmanFordLog
vertexUpdate vtx state messages =
  runVertexUpdate
    vtx
    state
    messages
    (bellmanFordUpdate vtx)
    emitOutgoing
    pathObserver

bellmanFordUpdate :: VertexContext -> [DistanceMsg] -> PathState -> Maybe PathState
bellmanFordUpdate vtx messages =
  tryImproveDistance
    (vcNodeId vtx)
    (mapMaybe (weightedCandidate vtx) messages)

weightedCandidate :: VertexContext -> DistanceMsg -> Maybe (Int, NodeId)
weightedCandidate vtx message =
  fmap
    (\weight -> (dmDistance message + weight, dmFrom message))
    (lookupIncomingWeight vtx (dmFrom message))

emitOutgoing :: VertexContext -> PathState -> [(NodeId, DistanceMsg)]
emitOutgoing vtx state =
  case psDistance state of
    Nothing -> []
    Just dist ->
      [ (to, DistanceMsg (vcNodeId vtx) dist)
        | to <- weightedOutNeighbors vtx
      ]
