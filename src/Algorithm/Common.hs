module Algorithm.Common
  ( UpdateM,
    validateWeightedGraph,
    extractPathResult,
    extractComponentResult,
    extractRankingsResult,
    extractLabelResult,
    reconstructPath,
    runVertexUpdate,
    stepResult,
  )
where

import Algorithm.Error (AlgorithmError (..))
import Control.Monad.State.Strict (StateT, runStateT)
import Control.Monad.Writer.Strict (Writer, runWriter)
import Data.List (sort)
import Data.Maybe (isJust)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Graph.Types
import Graph.VertexContext (VertexContext (vcNodeId))
import Pregel.Types

type UpdateM a = StateT VertexState (Writer [LogEntry]) a

validateWeightedGraph :: Graph -> Either AlgorithmError ()
validateWeightedGraph graph
  | null (graphEdges graph) =
      Left WeightedGraphRequired
  | all (isJust . edgeWeight) (graphEdges graph) =
      Right ()
  | otherwise =
      Left WeightedGraphRequired

extractPathResult :: Map NodeId VertexState -> RunConfig -> Result
extractPathResult states cfg =
  case rcTarget cfg of
    Nothing ->
      InputError MissingTarget
    Just target ->
      case Map.lookup target states of
        Nothing ->
          InputError (TargetNodeMissing target)
        Just vertexState ->
          case vsDistance vertexState of
            Nothing -> NoPath
            Just dist ->
              let path = reconstructPath states target (rcSource cfg)
               in if null path
                    then NoPath
                    else PathFound path dist

extractComponentResult :: Map NodeId VertexState -> RunConfig -> Result
extractComponentResult states cfg =
  case Map.lookup (rcSource cfg) states >>= vsLabel of
    Nothing ->
      InputError (TargetNodeMissing (rcSource cfg))
    Just componentLabel ->
      let members =
            sort
              [ nodeId
                | (nodeId, vertexState) <- Map.toList states,
                  vsLabel vertexState == Just componentLabel
              ]
       in ComponentFound componentLabel members

extractRankingsResult :: Map NodeId VertexState -> RunConfig -> Result
extractRankingsResult states _cfg =
  Rankings
    ( sort
        [ (nodeId, rank)
          | (nodeId, vertexState) <- Map.toList states,
            Just rank <- [vsRank vertexState]
        ]
    )

extractLabelResult :: Map NodeId VertexState -> RunConfig -> Result
extractLabelResult states _cfg =
  NodeLabels
    ( sort
        [ (nodeId, label)
          | (nodeId, vertexState) <- Map.toList states,
            Just label <- [vsLabel vertexState]
        ]
    )

reconstructPath :: Map NodeId VertexState -> NodeId -> NodeId -> [NodeId]
reconstructPath states target source = go target []
  where
    go node acc
      | node == source = node : acc
      | otherwise =
          case Map.lookup node states >>= vsPredecessor of
            Just predecessor -> go predecessor (node : acc)
            Nothing -> []

runVertexUpdate ::
  VertexContext ->
  VertexState ->
  [Message] ->
  UpdateM Bool ->
  (VertexContext -> VertexState -> [(NodeId, Message)]) ->
  VertexStepResult
runVertexUpdate vtx state _messages update emit =
  let nodeId = vcNodeId vtx
      ((changed, newState), logs) =
        runWriter (runStateT update state)
      outgoing =
        if changed
          then emit vtx newState
          else []
   in stepResult nodeId newState outgoing logs

stepResult ::
  NodeId ->
  VertexState ->
  [(NodeId, Message)] ->
  [LogEntry] ->
  VertexStepResult
stepResult nodeId newState outgoing logs =
  let sentLogs =
        [ MessageSent nodeId to msg
          | (to, msg) <- outgoing
        ]
   in VertexStepResult
        { vsrState = newState,
          vsrOutgoing = outgoing,
          vsrLogs = logs ++ sentLogs
        }
