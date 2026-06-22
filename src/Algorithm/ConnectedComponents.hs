module Algorithm.ConnectedComponents
  ( connectedComponentsSpec,
  )
where

import Algorithm.Common (UpdateM, extractComponentResult, runVertexUpdate)
import Algorithm.Types (AlgorithmSpec (..))
import Control.Monad.State.Strict (get, put)
import Control.Monad.Writer.Strict (tell)
import Graph.Types
import Graph.VertexContext (VertexContext (..), outNeighbors)
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
bootstrap cfg =
  let graph = rcGraph cfg
   in [ (to, MsgLabel nodeId)
        | nodeId <- graphNodes graph,
          (to, _) <- neighbors graph nodeId
      ]

vertexUpdate ::
  VertexContext ->
  VertexState ->
  [Message] ->
  VertexStepResult
vertexUpdate vtx state messages =
  let nodeId = vcNodeId vtx
   in runVertexUpdate vtx state messages (ccUpdate nodeId messages) emitOutgoing

ccUpdate :: NodeId -> [Message] -> UpdateM Bool
ccUpdate nodeId messages = do
  state <- get
  let currentLabel = maybe nodeId id (vsLabel state)
      incoming =
        [ label
          | MsgLabel label <- messages
        ]
      newLabel = minimum (currentLabel : incoming)
  if newLabel == currentLabel
    then pure False
    else do
      tell [VertexLabelUpdated nodeId newLabel]
      put state {vsLabel = Just newLabel}
      pure True

emitOutgoing :: VertexContext -> VertexState -> [(NodeId, Message)]
emitOutgoing vtx state =
  case vsLabel state of
    Nothing -> []
    Just label ->
      [ (to, MsgLabel label)
        | to <- outNeighbors vtx
      ]
