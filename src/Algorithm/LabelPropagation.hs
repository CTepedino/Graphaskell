module Algorithm.LabelPropagation
  ( labelPropagationGlobalSpec,
  )
where

import Algorithm.Common
  ( emitLabelMessages,
    extractLabelResult,
    labelBootstrap,
    labelMaxSupersteps,
    runVertexUpdate,
    tryRelabel,
  )
import Algorithm.Log (LabelLogEntry)
import Algorithm.Messages (LabelMsg (..))
import Algorithm.Observability (labelObserver)
import Algorithm.State (LabelState (..), emptyLabelState)
import Algorithm.Types (GlobalAlgorithmSpec (..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Graph.Types
import Graph.VertexContext (VertexContext (..))
import Pregel.Types

type LabelPropagationLog = LabelLogEntry LabelMsg

labelPropagationGlobalSpec :: GlobalAlgorithmSpec LabelState LabelMsg LabelPropagationLog
labelPropagationGlobalSpec =
  GlobalAlgorithmSpec
    { globalInitState = initState,
      globalDefaultState = emptyLabelState,
      globalBootstrap = bootstrap,
      globalVertexUpdate = vertexUpdate,
      globalExtractResult = extractLabelResult,
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
  VertexStepResult LabelState LabelMsg LabelPropagationLog
vertexUpdate vtx state messages =
  runVertexUpdate
    vtx
    state
    messages
    (lpaUpdate (vcNodeId vtx))
    emitLabelMessages
    labelObserver

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
