module Algorithm.LabelPropagation
  ( labelPropagationSpec,
  )
where

import Algorithm.Common
  ( VertexUpdate (..),
    extractLabelResult,
    labelBootstrap,
    runVertexUpdate,
    tryRelabel,
    emitLabelMessages,
  )
import Algorithm.Types (AlgorithmSpec (..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Graph.Types
import Graph.VertexContext (VertexContext (..))
import Pregel.Types

labelPropagationSpec :: AlgorithmSpec
labelPropagationSpec =
  AlgorithmSpec
    { specInitState = initState,
      specBootstrap = bootstrap,
      specVertexUpdate = vertexUpdate,
      specExtractResult = extractLabelResult
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
  runVertexUpdate vtx state messages (lpaUpdate (vcNodeId vtx)) emitLabelMessages

lpaUpdate :: NodeId -> [Message] -> VertexState -> VertexUpdate
lpaUpdate nodeId messages state =
  let currentLabel = maybe nodeId id (vsLabel state)
      newLabel = majorityLabel currentLabel messages
   in tryRelabel nodeId newLabel state

majorityLabel :: NodeId -> [Message] -> NodeId
majorityLabel self messages =
  let tallies :: Map NodeId Int
      tallies =
        Map.fromListWith
          (+)
          [ (label, 1)
            | label <-
                self : [incoming | MsgLabel incoming <- messages]
          ]
      maxVotes = maximum (Map.elems tallies)
      winners =
        Map.keys (Map.filter (== maxVotes) tallies)
   in minimum winners
