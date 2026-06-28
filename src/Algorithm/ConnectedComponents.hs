module Algorithm.ConnectedComponents
  ( connectedComponentsGlobalSpec,
  )
where

import Algorithm.Common
  ( extractComponentResult,
    labelVertexUpdate,
    labelsFromMessages,
    minimumWithSelf,
    tryRelabel,
  )
import Algorithm.Messages (LabelMsg)
import Algorithm.State (LabelState (..))
import Algorithm.Types (GlobalAlgorithmSpec, LabelLog, mkLabelGlobalSpec)
import Graph.Types (NodeId)
import Graph.VertexContext (VertexContext)
import Pregel.Types

connectedComponentsGlobalSpec :: GlobalAlgorithmSpec LabelState LabelMsg LabelLog
connectedComponentsGlobalSpec =
  mkLabelGlobalSpec vertexUpdate extractComponentResult

vertexUpdate ::
  VertexContext ->
  LabelState ->
  [LabelMsg] ->
  VertexStepResult LabelState LabelMsg
vertexUpdate =
  labelVertexUpdate ccUpdate

ccUpdate :: NodeId -> [LabelMsg] -> LabelState -> Maybe LabelState
ccUpdate nodeId messages state =
  let newLabel = minimumWithSelf (lsLabel state) (labelsFromMessages messages)
   in tryRelabel nodeId newLabel state
