module Algorithm.ConnectedComponents
  ( connectedComponentsSpec,
  )
where

import Algorithm.Common
  ( VertexUpdate (..),
    emitLabelMessages,
    extractComponentResult,
    labelBootstrap,
    labelsFromMessages,
    minimumWithSelf,
    runVertexUpdate,
    tryRelabel,
  )
import Algorithm.Types (AlgorithmSpec (..))
import Graph.Types
import Graph.VertexContext (VertexContext (..))
import Pregel.Types

connectedComponentsSpec :: AlgorithmSpec
connectedComponentsSpec =
  AlgorithmSpec
    { specInitState = initState,
      specBootstrap = bootstrap,
      specVertexUpdate = vertexUpdate,
      specExtractResult = extractComponentResult
    }

initState :: NodeId -> RunConfig -> VertexState
initState nodeId _cfg =
  initialVertexState {vsLabel = Just nodeId}

bootstrap :: RunConfig -> [(NodeId, Message)]
bootstrap = labelBootstrap

vertexUpdate ::
  VertexContext ->
  VertexState ->
  [Message] ->
  VertexStepResult
vertexUpdate vtx state messages =
  runVertexUpdate vtx state messages (ccUpdate (vcNodeId vtx)) emitLabelMessages

ccUpdate :: NodeId -> [Message] -> VertexState -> VertexUpdate
ccUpdate nodeId messages state =
  let currentLabel = maybe nodeId id (vsLabel state)
      newLabel = minimumWithSelf currentLabel (labelsFromMessages messages)
   in tryRelabel nodeId newLabel state
