module Algorithm.ConnectedComponents
  ( connectedComponentsSpec,
  )
where

import Algorithm.Common
  ( VertexUpdate (..),
    emitLabelMessages,
    extractComponentResult,
    labelBootstrap,
    labelMaxSupersteps,
    labelsFromMessages,
    minimumWithSelf,
    runVertexUpdate,
    tryRelabel,
  )
import Algorithm.State (LabelState (..), emptyLabelState)
import Algorithm.Types (AlgorithmSpec (..))
import Graph.Types
import Graph.VertexContext (VertexContext (..))
import Pregel.Types

connectedComponentsSpec :: AlgorithmSpec LabelState LabelMsg
connectedComponentsSpec =
  AlgorithmSpec
    { specInitState = initState,
      specDefaultState = emptyLabelState,
      specBootstrap = bootstrap,
      specVertexUpdate = vertexUpdate,
      specExtractResult = extractComponentResult,
      specMaxSupersteps = labelMaxSupersteps
    }

initState :: NodeId -> RunConfig -> LabelState
initState nodeId _cfg =
  LabelState nodeId

bootstrap :: RunConfig -> [(NodeId, LabelMsg)]
bootstrap = labelBootstrap

vertexUpdate ::
  VertexContext ->
  LabelState ->
  [LabelMsg] ->
  VertexStepResult LabelState LabelMsg
vertexUpdate vtx state messages =
  runVertexUpdate vtx state messages (ccUpdate (vcNodeId vtx)) emitLabelMessages

ccUpdate :: NodeId -> [LabelMsg] -> LabelState -> VertexUpdate LabelState LabelMsg
ccUpdate nodeId messages state =
  let newLabel = minimumWithSelf (lsLabel state) (labelsFromMessages messages)
   in tryRelabel nodeId newLabel state
