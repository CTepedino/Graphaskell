module Algorithm.Dijkstra
  ( dijkstraSpec,
    validateWeightedGraph,
  )
where

import Algorithm.Common (extractPathResult, lookupEdgeWeight)
import Algorithm.Error (AlgorithmError (..))
import Algorithm.Types (AlgorithmSpec (..))
import Control.Monad.State.Strict (StateT, get, put, runStateT)
import Control.Monad.Writer.Strict (Writer, runWriter, tell)
import Data.List (minimumBy)
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
import Graph.Types
import Pregel.Types

dijkstraSpec :: AlgorithmSpec
dijkstraSpec =
  AlgorithmSpec
    { specInitState = initState,
      specBootstrap = bootstrap,
      specVertexUpdate = vertexUpdate,
      specExtractResult = extractPathResult
    }

validateWeightedGraph :: Graph -> Either AlgorithmError ()
validateWeightedGraph graph
  | all (maybe False (> 0) . edgeWeight) (graphEdges graph) =
      Right ()
  | otherwise =
      Left DijkstraRequiresWeightedGraph

initState :: NodeId -> RunConfig -> VertexState
initState nodeId cfg
  | nodeId == rcSource cfg =
      initialVertexState {vsDistance = Just 0}
  | otherwise =
      initialVertexState

bootstrap :: RunConfig -> [(NodeId, Message)]
bootstrap cfg =
  [ (to, MsgDistance (rcSource cfg) 0)
    | (to, Just _) <- neighbors (rcGraph cfg) (rcSource cfg)
  ]

vertexUpdate ::
  Graph ->
  VertexStates ->
  NodeId ->
  VertexState ->
  [Message] ->
  VertexStepResult
vertexUpdate graph _allStates nodeId state messages =
  let ((changed, newState), logs) =
        runWriter (runStateT (dijkstraUpdate graph nodeId messages) state)
      outgoing =
        if changed
          then emitOutgoing graph nodeId newState
          else []
      sentLogs =
        [ MessageSent nodeId to msg
          | (to, msg) <- outgoing
        ]
   in VertexStepResult
        { vsrState = newState,
          vsrOutgoing = outgoing,
          vsrLogs = logs ++ sentLogs,
          vsrChanged = changed
        }

type UpdateM a = StateT VertexState (Writer [LogEntry]) a

dijkstraUpdate :: Graph -> NodeId -> [Message] -> UpdateM Bool
dijkstraUpdate graph nodeId messages = do
  state <- get
  let candidates =
        mapMaybe
          (candidate graph nodeId)
          messages
  case candidates of
    [] -> pure False
    _ -> do
      let (newDist, predecessor) =
            minimumBy (comparing fst) candidates
      case vsDistance state of
        Just current | newDist >= current -> pure False
        _ -> do
          tell [VertexUpdated nodeId newDist]
          put
            state
              { vsDistance = Just newDist,
                vsPredecessor = Just predecessor
              }
          pure True

candidate :: Graph -> NodeId -> Message -> Maybe (Int, NodeId)
candidate _graph _nodeId (MsgVisit _) = Nothing
candidate graph nodeId (MsgDistance from dist) = do
  weight <- lookupEdgeWeight graph from nodeId
  if weight > 0
    then Just (dist + weight, from)
    else Nothing

emitOutgoing :: Graph -> NodeId -> VertexState -> [(NodeId, Message)]
emitOutgoing graph nodeId state =
  case vsDistance state of
    Nothing -> []
    Just dist ->
      [ (to, MsgDistance nodeId dist)
        | (to, Just _) <- neighbors graph nodeId
      ]
