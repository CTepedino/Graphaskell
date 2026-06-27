module Algorithm.LabelPropagation
  ( labelPropagationSpec,
  )
where

import Algorithm.Common
  ( VertexUpdate (..),
    extractLabelResult,
    labelBootstrap,
    labelMaxSupersteps,
    runVertexUpdate,
    tryRelabel,
    emitLabelMessages,
  )
import Algorithm.State (LabelState (..), emptyLabelState)
import Algorithm.Types (AlgorithmSpec (..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Graph.Types
import Graph.VertexContext (VertexContext (..))
import Pregel.Types

labelPropagationSpec :: AlgorithmSpec LabelState LabelMsg
labelPropagationSpec =
  AlgorithmSpec
    { specInitState = initState,
      specDefaultState = emptyLabelState,
      specBootstrap = bootstrap,
      specVertexUpdate = vertexUpdate,
      specExtractResult = extractLabelResult,
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
  runVertexUpdate vtx state messages (lpaUpdate (vcNodeId vtx)) emitLabelMessages

lpaUpdate :: NodeId -> [LabelMsg] -> LabelState -> VertexUpdate LabelState LabelMsg
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
