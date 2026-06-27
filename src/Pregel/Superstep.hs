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
  Maybe (VertexStepResult state msg log)
applyVertexUpdate spec contexts states nodeId messages = do
  vtx <- Map.lookup nodeId contexts
  let state = Map.findWithDefault (specDefaultState spec) nodeId states
  pure (specVertexUpdate spec vtx state messages)

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
  AlgorithmSpec state msg log ->
  VertexContexts ->
  VertexStates state ->
  (NodeId -> [msg]) ->
  [NodeId] ->
  Either PregelError (SuperstepResult state msg log)
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
