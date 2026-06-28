module Algorithm.BellmanFord
  ( bellmanFordPathSpec,
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
import Algorithm.State (PathState, emptyPathState)
import Algorithm.Types (PathAlgorithmSpec (..))
import Data.Maybe (isJust, mapMaybe)
import Graph.Types (NodeId)
import Graph.VertexContext
  ( VertexContext (..),
    lookupIncomingWeight,
    weightedOutNeighbors,
  )
import Pregel.Types

bellmanFordPathSpec :: PathAlgorithmSpec
bellmanFordPathSpec =
  PathAlgorithmSpec
    { psInitState = pathInitState,
      psDefaultState = emptyPathState,
      psBootstrap = pathBootstrap isJust,
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
    (bellmanFordUpdate vtx)
    (emitDistanceMessages weightedOutNeighbors)

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
