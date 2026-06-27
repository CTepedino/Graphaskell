module Algorithm.BellmanFord
  ( bellmanFordSpec,
  )
where

import Algorithm.Common
  ( VertexUpdate (..),
    extractPathResult,
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
import Data.Maybe (isJust, mapMaybe)
import Graph.Types (NodeId)
import Graph.VertexContext
  ( VertexContext (..),
    lookupIncomingWeight,
    weightedOutNeighbors,
  )
import Pregel.Types

type BellmanFordLog = PathLogEntry DistanceMsg

bellmanFordSpec :: AlgorithmSpec PathState DistanceMsg BellmanFordLog
bellmanFordSpec =
  AlgorithmSpec
    { specInitState = pathInitState,
      specDefaultState = emptyPathState,
      specBootstrap = pathBootstrap isJust,
      specVertexUpdate = vertexUpdate,
      specExtractResult = extractPathResult,
      specMaxSupersteps = pathMaxSupersteps
    }

vertexUpdate ::
  VertexContext ->
  PathState ->
  [DistanceMsg] ->
  VertexStepResult PathState DistanceMsg BellmanFordLog
vertexUpdate vtx state messages =
  runVertexUpdate vtx state messages (bellmanFordUpdate vtx) emitOutgoing

bellmanFordUpdate :: VertexContext -> [DistanceMsg] -> PathState -> VertexUpdate PathState DistanceMsg BellmanFordLog
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
