module Algorithm.BellmanFord
  ( bellmanFordSpec,
  )
where

import Algorithm.Common
  ( atLeastOneSuperstep,
    emitDistanceMessages,
    extractPathResult,
    pathBootstrap,
    pathInitState,
    runVertexUpdate,
    tryImproveDistance,
  )
import Algorithm.Messages (DistanceMsg (..))
import Algorithm.Observability (pathObserver)
import Algorithm.State (PathState, emptyPathState)
import Algorithm.Types (AlgorithmSpec (..), PathLog)
import Data.Maybe (isJust, mapMaybe)
import Graph.Types (Distance, NodeId, distancePlusWeight)
import Graph.VertexContext
  ( VertexContext (..),
    lookupIncomingWeight,
    weightedOutNeighbors,
  )
import Pregel.Types

bellmanFordSpec :: AlgorithmSpec PathState DistanceMsg PathLog
bellmanFordSpec =
  AlgorithmSpec
    { specInitState = pathInitState,
      specDefaultState = emptyPathState,
      specBootstrap = pathBootstrap isJust,
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
    (bellmanFordUpdate vtx)
    (emitDistanceMessages weightedOutNeighbors)

bellmanFordUpdate :: VertexContext -> [DistanceMsg] -> PathState -> Maybe PathState
bellmanFordUpdate vtx messages =
  tryImproveDistance
    (vcNodeId vtx)
    (mapMaybe (weightedCandidate vtx) messages)

weightedCandidate :: VertexContext -> DistanceMsg -> Maybe (Distance, NodeId)
weightedCandidate vtx message =
  fmap
    (\weight -> (distancePlusWeight (dmDistance message) weight, dmFrom message))
    (lookupIncomingWeight vtx (dmFrom message))
