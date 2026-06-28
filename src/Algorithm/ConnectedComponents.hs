module Algorithm.ConnectedComponents
  ( connectedComponentsSpec,
  )
where

import Algorithm.Common
  ( extractComponentResult,
    labelVertexUpdateAlwaysEmit,
    labelsFromMessages,
    minimumWithSelf,
    tryRelabel,
  )
import Algorithm.Messages (LabelMsg)
import Algorithm.State (LabelState (..))
import Algorithm.Types (AlgorithmSpec, LabelLog, mkLabelSpec)
import Graph.Types (NodeId)
import Graph.VertexContext (VertexContext)
import Pregel.Types

connectedComponentsSpec :: AlgorithmSpec LabelState LabelMsg LabelLog
connectedComponentsSpec =
  mkLabelSpec vertexUpdate extractComponentResult

vertexUpdate ::
  VertexContext ->
  LabelState ->
  [LabelMsg] ->
  VertexStepResult LabelState LabelMsg
vertexUpdate =
  labelVertexUpdateAlwaysEmit ccUpdate

ccUpdate :: NodeId -> [LabelMsg] -> LabelState -> Maybe LabelState
ccUpdate nodeId messages state =
  let newLabel = minimumWithSelf (lsLabel state) (labelsFromMessages messages)
   in tryRelabel nodeId newLabel state
