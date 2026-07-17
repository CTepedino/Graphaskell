module Pregel.Superstep
  ( SuperstepResult (..),
    initialVertexStates,
    mergeSuperstepOutcomes,
    mkSuperstepLog,
    processVertex,
  )
where

import Algorithm.Log (MessageLog (..), messageSentLogs)
import Algorithm.Types (AlgorithmSpec (..))
import qualified Data.Map.Strict as Map
import Graph.Types (NodeId, ValidGraph, graphNodes)
import Graph.VertexContext (VertexContexts)
import Pregel.Error (PregelError (..))
import Pregel.Types

data SuperstepResult state msg log = SuperstepResult
  { ssNewStates :: VertexStates state,
    ssOutgoing :: [(NodeId, msg)],
    ssEntries :: [log],
    ssStateChanged :: Bool
  }

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

mergeUpdatedStates ::
  VertexStates state ->
  [(NodeId, state)] ->
  VertexStates state
mergeUpdatedStates states updates =
  foldr (uncurry Map.insert) states updates

initialVertexStates ::
  AlgorithmSpec state msg log ->
  RunConfig ->
  ValidGraph ->
  VertexStates state
initialVertexStates spec cfg graph =
  Map.fromList
    [ (nodeId, specInitState spec nodeId cfg)
      | nodeId <- graphNodes graph
    ]

processVertex ::
  (MessageLog msg log, Eq state) =>
  Bool ->
  AlgorithmSpec state msg log ->
  VertexContexts ->
  VertexStates state ->
  NodeId ->
  [msg] ->
  Either PregelError (NodeId, VertexStepResult state msg, [log])
processVertex tracing spec contexts states nodeId messages =
  case applyVertexUpdate spec contexts states nodeId messages of
    Nothing ->
      Left (MissingVertexContext nodeId)
    Just result ->
      let oldState = Map.findWithDefault (specDefaultState spec) nodeId states
          logs =
            if tracing
              then collectVertexLogs spec nodeId oldState result
              else []
       in Right (nodeId, result, logs)

mergeSuperstepOutcomes ::
  Eq state =>
  VertexStates state ->
  [(NodeId, VertexStepResult state msg, [log])] ->
  SuperstepResult state msg log
mergeSuperstepOutcomes states outcomes =
  SuperstepResult
    { ssNewStates =
        mergeUpdatedStates
          states
          [ (nodeId, vsrState result)
            | (nodeId, result, _) <- outcomes
          ],
      ssOutgoing =
        concatMap (vsrOutgoing . (\(_, result, _) -> result)) outcomes,
      ssEntries = concatMap (\(_, _, logs) -> logs) outcomes,
      ssStateChanged =
        any
          ( \(nodeId, VertexStepResult newState _, _) ->
              case Map.lookup nodeId states of
                Just oldState ->
                  newState /= oldState
                Nothing ->
                  True
          )
          outcomes
    }

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
