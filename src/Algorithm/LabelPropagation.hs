module Algorithm.LabelPropagation
  ( labelPropagationSpec,
    labelPropagationStable,
  )
where

import Algorithm.Common
  ( extractLabelResult,
    labelVertexUpdateAlwaysEmit,
    mkLabelSpec,
    tryRelabel,
  )
import Algorithm.Messages (LabelMsg (..))
import Algorithm.State (LabelState (..))
import Algorithm.Types (AlgorithmSpec, LabelLog)
import qualified Data.Map.Strict as Map
import Graph.Types (Edge (..), NodeId, ValidGraph, graphEdges, graphNodes)
import Graph.VertexContext (VertexContext)
import Pregel.Types

labelPropagationSpec :: AlgorithmSpec LabelState LabelMsg LabelLog
labelPropagationSpec =
  mkLabelSpec vertexUpdate extractLabelResult

labelPropagationStable :: ValidGraph -> [(NodeId, NodeId)] -> Bool
labelPropagationStable graph pairs =
  let states = Map.fromList [ (nodeId, LabelState label) | (nodeId, label) <- pairs ]
      isStable nodeId =
        case lpaUpdate nodeId (neighborLabelMessages graph nodeId states) (states Map.! nodeId) of
          Nothing ->
            True
          Just _ ->
            False
   in all isStable (graphNodes graph)
  where
    neighborLabelMessages graph' nodeId states' =
      [ LabelMsg (lsLabel (states' Map.! from))
        | from <- incomingNeighbors graph' nodeId
      ]

incomingNeighbors :: ValidGraph -> NodeId -> [NodeId]
incomingNeighbors graph nodeId =
  [ edgeFrom edge
    | edge <- graphEdges graph,
      edgeTo edge == nodeId
  ]

vertexUpdate ::
  VertexContext ->
  LabelState ->
  [LabelMsg] ->
  VertexStepResult LabelState LabelMsg
vertexUpdate =
  labelVertexUpdateAlwaysEmit lpaUpdate

lpaUpdate :: NodeId -> [LabelMsg] -> LabelState -> Maybe LabelState
lpaUpdate nodeId messages state =
  let newLabel = majorityLabel (lsLabel state) messages
   in tryRelabel nodeId newLabel state

majorityLabel :: NodeId -> [LabelMsg] -> NodeId
majorityLabel self messages =
  let labels = self : [lmLabel message | message <- messages]
      tallies =
        Map.fromListWith ((+) @Int) [(label, 1) | label <- labels]
      maxVotes = maximum (Map.elems tallies)
      winners =
        [ label
          | (label, votes) <- Map.toList tallies,
            votes == maxVotes
        ]
   in minimum winners
