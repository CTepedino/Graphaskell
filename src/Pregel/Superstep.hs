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

import Algorithm.Types (AlgorithmSpec (..))
import qualified Data.Map.Strict as Map
import Graph.Types (Graph, NodeId, graphNodes)
import Graph.VertexContext (VertexContexts)
import Pregel.Error (PregelError (..))
import Pregel.Types

data SuperstepResult = SuperstepResult
  { ssNewStates :: VertexStates,
    ssOutgoing :: [(NodeId, Message)],
    ssEntries :: [LogEntry]
  }
  deriving (Eq, Show)

applyVertexUpdate ::
  AlgorithmSpec ->
  VertexContexts ->
  VertexStates ->
  NodeId ->
  [Message] ->
  Maybe VertexStepResult
applyVertexUpdate spec contexts states nodeId messages = do
  vtx <- Map.lookup nodeId contexts
  let state = Map.findWithDefault initialVertexState nodeId states
  pure (specVertexUpdate spec vtx state messages)

activeVerticesWithMessages :: MessageQueues -> [NodeId]
activeVerticesWithMessages =
  Map.keys . Map.filter (not . null)

enqueueMessages :: MessageQueues -> [(NodeId, Message)] -> MessageQueues
enqueueMessages =
  foldr
    ( \(nodeId, message) queues ->
        Map.insertWith (++) nodeId [message] queues
    )

mergeUpdatedStates ::
  VertexStates ->
  [(NodeId, VertexState)] ->
  VertexStates
mergeUpdatedStates states updates =
  foldr (uncurry Map.insert) states updates

initialVertexStates ::
  AlgorithmSpec ->
  RunConfig ->
  Graph ->
  VertexStates
initialVertexStates spec cfg graph =
  Map.fromList
    [ (nodeId, specInitState spec nodeId cfg)
      | nodeId <- graphNodes graph
    ]

processActiveVertices ::
  AlgorithmSpec ->
  VertexContexts ->
  VertexStates ->
  (NodeId -> [Message]) ->
  [NodeId] ->
  Either PregelError SuperstepResult
processActiveVertices spec contexts states messageFor actives =
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
                  | (nodeId, result) <- outcomes
                ],
            ssOutgoing =
              concatMap (vsrOutgoing . snd) outcomes,
            ssEntries = concatMap (vsrLogs . snd) outcomes
          }
  where
    processOne nodeId =
      case applyVertexUpdate spec contexts states nodeId (messageFor nodeId) of
        Nothing ->
          Left (MissingVertexContext nodeId)
        Just result ->
          Right (nodeId, result)

mkSuperstepLog ::
  Int ->
  [NodeId] ->
  [(NodeId, Message)] ->
  [LogEntry] ->
  SuperstepLog
mkSuperstepLog step actives outgoing entries =
  SuperstepLog
    { sslStep = step,
      sslActiveVertices = length actives,
      sslMessagesSent = length outgoing,
      sslEntries = entries
    }
