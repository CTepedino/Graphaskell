module Algorithm.LabelPropagation
  ( labelPropagationSpec,
    labelPropagationStable,
  )
where

import Algorithm.Common
  ( extractLabelResult,
    labelVertexUpdate,
    tryRelabel,
  )
import Algorithm.Messages (LabelMsg (..))
import Algorithm.State (LabelState (..))
import Algorithm.Types (AlgorithmSpec, LabelLog, mkLabelSpec)
import qualified Data.Map.Strict as Map
import Graph.Types (NodeId, ValidGraph, graphNodes, neighbors)
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
      [ LabelMsg (lsLabel (states' Map.! to))
        | (to, _) <- neighbors graph' nodeId
      ]

vertexUpdate ::
  VertexContext ->
  LabelState ->
  [LabelMsg] ->
  VertexStepResult LabelState LabelMsg
vertexUpdate =
  labelVertexUpdate lpaUpdate

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
   in foldl min self winners
