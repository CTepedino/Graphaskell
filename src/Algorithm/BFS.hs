module Algorithm.BFS
  ( bfsSpec,
  )
where

import Algorithm.Common (extractPathResult)
import Algorithm.Types (AlgorithmSpec (..))
import Control.Monad.State.Strict (StateT, get, put, runStateT)
import Control.Monad.Writer.Strict (Writer, runWriter, tell)
import Data.List (minimumBy)
import Data.Ord (comparing)
import Graph.Types
import Pregel.Types

bfsSpec :: AlgorithmSpec
bfsSpec =
  AlgorithmSpec
    { specInitState = initState,
      specBootstrap = bootstrap,
      specVertexUpdate = vertexUpdate,
      specExtractResult = extractPathResult
    }

initState :: NodeId -> RunConfig -> VertexState
initState nodeId cfg
  | nodeId == rcSource cfg =
      initialVertexState {vsDistance = Just 0}
  | otherwise =
      initialVertexState

bootstrap :: RunConfig -> [(NodeId, Message)]
bootstrap cfg =
  [ (to, MsgDistance (rcSource cfg) 0)
    | (to, _) <- neighbors (rcGraph cfg) (rcSource cfg)
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
        runWriter (runStateT (bfsUpdate nodeId messages) state)
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

bfsUpdate :: NodeId -> [Message] -> UpdateM Bool
bfsUpdate nodeId messages = do
  state <- get
  let candidates =
        [ (dist + 1, from)
          | MsgDistance from dist <- messages
        ]
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

emitOutgoing :: Graph -> NodeId -> VertexState -> [(NodeId, Message)]
emitOutgoing graph nodeId state =
  case vsDistance state of
    Nothing -> []
    Just dist ->
      [ (to, MsgDistance nodeId dist)
        | (to, _) <- neighbors graph nodeId
      ]
