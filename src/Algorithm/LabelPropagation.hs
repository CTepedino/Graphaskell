module Algorithm.LabelPropagation
  ( labelPropagationGlobalSpec,
  )
where

import Algorithm.Common
  ( extractLabelResult,
    labelVertexUpdate,
    tryRelabel,
  )
import Algorithm.Messages (LabelMsg (..))
import Algorithm.State (LabelState (..))
import Algorithm.Types (GlobalAlgorithmSpec, LabelLog, mkLabelGlobalSpec)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Graph.Types (NodeId)
import Graph.VertexContext (VertexContext)
import Pregel.Types

labelPropagationGlobalSpec :: GlobalAlgorithmSpec LabelState LabelMsg LabelLog
labelPropagationGlobalSpec =
  mkLabelGlobalSpec vertexUpdate extractLabelResult

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
  let tallies :: Map NodeId Int
      tallies =
        Map.fromListWith
          (+)
          [ (label, 1)
            | label <-
                self : [lmLabel message | message <- messages]
          ]
      maxVotes = maximum (Map.elems tallies)
      winners =
        Map.keys (Map.filter (== maxVotes) tallies)
   in minimum winners
