module Algorithm.LabelPropagation
  ( labelPropagationSpec,
  )
where

import Algorithm.Common (UpdateM, extractLabelResult, runVertexUpdate)
import Algorithm.Types (AlgorithmSpec (..))
import Control.Monad.State.Strict (get, put)
import Control.Monad.Writer.Strict (tell)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Graph.Types
import Graph.VertexContext (VertexContext (..), outNeighbors)
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
   in runVertexUpdate vtx state messages (lpaUpdate nodeId messages) emitOutgoing

lpaUpdate :: NodeId -> [Message] -> UpdateM Bool
lpaUpdate nodeId messages = do
  state <- get
  let currentLabel = maybe nodeId id (vsLabel state)
      newLabel = majorityLabel currentLabel messages
  if newLabel == currentLabel
    then pure False
    else do
      tell [VertexLabelUpdated nodeId newLabel]
      put state {vsLabel = Just newLabel}
      pure True

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

emitOutgoing :: VertexContext -> VertexState -> [(NodeId, Message)]
emitOutgoing vtx state =
  case vsLabel state of
    Nothing -> []
    Just label ->
      [ (to, MsgLabel label)
        | to <- outNeighbors vtx
      ]
