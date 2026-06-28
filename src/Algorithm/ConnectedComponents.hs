module Algorithm.ConnectedComponents
  ( connectedComponentsGlobalSpec,
  )
where

import Algorithm.Common
  ( emitLabelMessages,
    extractComponentResult,
    labelBootstrap,
    labelMaxSupersteps,
    labelsFromMessages,
    minimumWithSelf,
    runVertexUpdate,
    tryRelabel,
  )
import Algorithm.Log (LabelLogEntry)
import Algorithm.Messages (LabelMsg)
import Algorithm.Observability (labelObserver)
import Algorithm.State (LabelState (..), emptyLabelState)
import Algorithm.Types (GlobalAlgorithmSpec (..))
import Graph.Types
import Graph.VertexContext (VertexContext (..))
import Pregel.Types

type ComponentLog = LabelLogEntry LabelMsg

connectedComponentsGlobalSpec :: GlobalAlgorithmSpec LabelState LabelMsg ComponentLog
connectedComponentsGlobalSpec =
  GlobalAlgorithmSpec
    { globalInitState = initState,
      globalDefaultState = emptyLabelState,
      globalBootstrap = bootstrap,
      globalVertexUpdate = vertexUpdate,
      globalExtractResult = extractComponentResult,
      globalMaxSupersteps = labelMaxSupersteps,
      globalObserveStep = labelObserver
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
  VertexStepResult LabelState LabelMsg ComponentLog
vertexUpdate vtx state messages =
  runVertexUpdate
    vtx
    state
    messages
    (ccUpdate (vcNodeId vtx))
    emitLabelMessages
    labelObserver

ccUpdate :: NodeId -> [LabelMsg] -> LabelState -> Maybe LabelState
ccUpdate nodeId messages state =
  let newLabel = minimumWithSelf (lsLabel state) (labelsFromMessages messages)
   in tryRelabel nodeId newLabel state
