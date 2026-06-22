module Algorithm.DFS
  ( dfsSpec,
  )
where

import Algorithm.Common
  ( extractVisitedResult,
    firstUnvisitedNeighbor,
  )
import Algorithm.Types (AlgorithmSpec (..))
import Control.Monad.State.Strict (StateT, get, put, runStateT)
import Control.Monad.Writer.Strict (Writer, runWriter, tell)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Graph.Types
import Pregel.Types

dfsSpec :: AlgorithmSpec
dfsSpec =
  AlgorithmSpec
    { specInitState = initState,
      specBootstrap = bootstrap,
      specVertexUpdate = vertexUpdate,
      specExtractResult = extractVisitedResult
    }

initState :: NodeId -> RunConfig -> VertexState
initState nodeId cfg
  | nodeId == rcSource cfg =
      initialVertexState
        { vsVisited = True,
          vsDistance = Just 0
        }
  | otherwise =
      initialVertexState

bootstrap :: RunConfig -> [(NodeId, Message)]
bootstrap cfg =
  let states = initialStates cfg
   in case firstUnvisitedNeighbor (rcGraph cfg) states (rcSource cfg) of
        Nothing -> []
        Just to -> [(to, MsgVisit (rcSource cfg))]

initialStates :: RunConfig -> Map NodeId VertexState
initialStates cfg =
  Map.fromList
    [ (nodeId, initState nodeId cfg)
      | nodeId <- graphNodes (rcGraph cfg)
    ]

vertexUpdate ::
  Graph ->
  VertexStates ->
  NodeId ->
  VertexState ->
  [Message] ->
  VertexStepResult
vertexUpdate graph allStates nodeId state messages =
  let ((changed, newState), logs) =
        runWriter (runStateT (dfsUpdate graph allStates nodeId messages) state)
      updatedStates = Map.insert nodeId newState allStates
      outgoing =
        if changed
          then emitOutgoing graph updatedStates nodeId newState
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

dfsUpdate ::
  Graph ->
  VertexStates ->
  NodeId ->
  [Message] ->
  UpdateM Bool
dfsUpdate _graph allStates nodeId messages = do
  state <- get
  if vsVisited state
    then pure False
    else
      case choosePredecessor messages of
        Nothing -> pure False
        Just from -> do
          let depth =
                case Map.lookup from allStates >>= vsDistance of
                  Just parentDepth -> parentDepth + 1
                  Nothing -> 1
          tell [VertexUpdated nodeId depth]
          put
            state
              { vsVisited = True,
                vsPredecessor = Just from,
                vsDistance = Just depth
              }
          pure True

choosePredecessor :: [Message] -> Maybe NodeId
choosePredecessor messages =
  case [from | MsgVisit from <- messages] of
    [] -> Nothing
    senders -> Just (minimum senders)

emitOutgoing ::
  Graph ->
  VertexStates ->
  NodeId ->
  VertexState ->
  [(NodeId, Message)]
emitOutgoing graph states nodeId _state =
  case firstUnvisitedNeighbor graph states nodeId of
    Nothing -> []
    Just to -> [(to, MsgVisit nodeId)]
