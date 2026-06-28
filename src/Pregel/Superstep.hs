module Pregel.Superstep
  ( SuperstepResult (..),
    applyVertexUpdate,
    activeVerticesWithMessages,
    enqueueMessages,
    initialVertexStates,
    mergeUpdatedStates,
    mkSuperstepLog,
    processActiveVertices,
  )
where

import Algorithm.Log (MessageLog (..), messageSentLogs)
import Algorithm.Types (AlgorithmSpec (..))
import qualified Data.Map.Strict as Map
import Graph.Types (Graph, NodeId, graphNodes)
import Graph.VertexContext (VertexContexts)
import Pregel.Error (PregelError (..))
import Pregel.Types

data SuperstepResult state msg log = SuperstepResult
  { ssNewStates :: VertexStates state,
    ssOutgoing :: [(NodeId, msg)],
    ssEntries :: [log]
  }
  deriving (Eq, Show)

applyVertexUpdate ::
  AlgorithmSpec state msg log ->
  VertexContexts ->
  VertexStates state ->
  NodeId ->
  [msg] ->
  Maybe (VertexStepResult state msg)
applyVertexUpdate spec contexts states nodeId messages = do
  vtx <- Map.lookup nodeId contexts
  let state = Map.findWithDefault (specDefaultState spec) nodeId states
  pure (specVertexUpdate spec vtx state messages)

collectVertexLogs ::
  MessageLog msg log =>
  AlgorithmSpec state msg log ->
  NodeId ->
  state ->
  VertexStepResult state msg ->
  [log]
collectVertexLogs spec nodeId oldState result =
  specObserveStep spec nodeId oldState (vsrState result) (vsrOutgoing result)
    ++ messageSentLogs nodeId (vsrOutgoing result)

activeVerticesWithMessages :: MessageQueues msg -> [NodeId]
activeVerticesWithMessages =
  Map.keys . Map.filter (not . null)

enqueueMessages :: MessageQueues msg -> [(NodeId, msg)] -> MessageQueues msg
enqueueMessages =
  foldr
    ( \(nodeId, message) queues ->
        Map.insertWith (++) nodeId [message] queues
    )

mergeUpdatedStates ::
  VertexStates state ->
  [(NodeId, state)] ->
  VertexStates state
mergeUpdatedStates states updates =
  foldr (uncurry Map.insert) states updates

initialVertexStates ::
  AlgorithmSpec state msg log ->
  RunConfig ->
  Graph ->
  VertexStates state
initialVertexStates spec cfg graph =
  Map.fromList
    [ (nodeId, specInitState spec nodeId cfg)
      | nodeId <- graphNodes graph
    ]

processActiveVertices ::
  MessageLog msg log =>
  Bool ->
  AlgorithmSpec state msg log ->
  VertexContexts ->
  VertexStates state ->
  (NodeId -> [msg]) ->
  [NodeId] ->
  Either PregelError (SuperstepResult state msg log)
processActiveVertices tracing spec contexts states messageFor actives =
  case mapM processOne actives of
    Left err ->
      Left err
    Right outcomes ->
      Right
        SuperstepResult
          { ssNewStates =
              mergeUpdatedStates
                states
                [ (nodeId, vsrState result)
                  | (nodeId, result, _) <- outcomes
                ],
            ssOutgoing =
              concatMap (vsrOutgoing . (\(_, result, _) -> result)) outcomes,
            ssEntries = concatMap (\(_, _, logs) -> logs) outcomes
          }
  where
    processOne nodeId =
      case applyVertexUpdate spec contexts states nodeId (messageFor nodeId) of
        Nothing ->
          Left (MissingVertexContext nodeId)
        Just result ->
          let oldState = Map.findWithDefault (specDefaultState spec) nodeId states
              logs =
                if tracing
                  then collectVertexLogs spec nodeId oldState result
                  else []
           in Right (nodeId, result, logs)

mkSuperstepLog ::
  Int ->
  [NodeId] ->
  [(NodeId, msg)] ->
  [log] ->
  SuperstepLog log
mkSuperstepLog step actives outgoing entries =
  SuperstepLog
    { sslStep = step,
      sslActiveVertices = length actives,
      sslMessagesSent = length outgoing,
      sslEntries = entries
    }
