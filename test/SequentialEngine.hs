module SequentialEngine
  ( runSequential,
  )
where

import Algorithm.Types (AlgorithmSpec (..))
import qualified Data.Map.Strict as Map
import Graph.Types (NodeId, graphNodes)
import Graph.VertexContext (VertexContexts, buildVertexContexts)
import Pregel.Engine (PregelRun (..))
import Pregel.Types

runSequential :: RunConfig -> AlgorithmSpec -> PregelRun
runSequential cfg spec =
  let graph = rcGraph cfg
      contexts = buildVertexContexts graph
      nodes = graphNodes graph
      initialStates =
        Map.fromList
          [ (nodeId, specInitState spec nodeId cfg)
            | nodeId <- nodes
          ]
      initialQueues = enqueueAll Map.empty (specBootstrap spec cfg)
      (finalStates, logs, steps, maxStepsReached) =
        loop contexts spec cfg 0 initialStates initialQueues
   in PregelRun
        { prSupersteps = steps,
          prLogs = logs,
          prResult = specExtractResult spec finalStates cfg,
          prMaxStepsReached = maxStepsReached
        }

data LoopInput = LoopInput
  { liStep :: Int,
    liStates :: VertexStates,
    liQueues :: MessageQueues
  }

loop ::
  VertexContexts ->
  AlgorithmSpec ->
  RunConfig ->
  Int ->
  VertexStates ->
  MessageQueues ->
  (VertexStates, [SuperstepLog], Int, Bool)
loop contexts spec cfg step states queues =
  let initial = LoopInput step states queues
      (logs, finalInput) = unfoldLoop (superstepOnce contexts spec cfg) initial
      maxStepsReached = liStep finalInput >= rcMaxSteps cfg
   in (liStates finalInput, logs, liStep finalInput, maxStepsReached)

unfoldLoop :: (s -> Maybe (a, s)) -> s -> ([a], s)
unfoldLoop stepFn seed = go seed []
  where
    go state acc =
      case stepFn state of
        Nothing -> (reverse acc, state)
        Just (value, nextState) -> go nextState (value : acc)

superstepOnce ::
  VertexContexts ->
  AlgorithmSpec ->
  RunConfig ->
  LoopInput ->
  Maybe (SuperstepLog, LoopInput)
superstepOnce contexts spec cfg input
  | liStep input >= rcMaxSteps cfg =
      Nothing
  | null (activeVertices (liQueues input)) =
      Nothing
  | otherwise =
      let step = liStep input
          actives = activeVertices (liQueues input)
          (newStates, outgoing, entries) =
            processActive
              contexts
              spec
              actives
              (liStates input)
              (liQueues input)
          logEntry =
            SuperstepLog
              { sslStep = step,
                sslActiveVertices = length actives,
                sslMessagesSent = length outgoing,
                sslEntries = entries
              }
          nextInput =
            LoopInput
              { liStep = step + 1,
                liStates = newStates,
                liQueues = enqueueAll Map.empty outgoing
              }
       in Just (logEntry, nextInput)

processActive ::
  VertexContexts ->
  AlgorithmSpec ->
  [NodeId] ->
  VertexStates ->
  MessageQueues ->
  (VertexStates, [(NodeId, Message)], [LogEntry])
processActive contexts spec actives states queues =
  let (newStates, outgoingChunks, entryChunks) =
        foldr
          (processVertex contexts spec states queues)
          (states, [], [])
          actives
   in ( newStates,
        concat (reverse outgoingChunks),
        concat (reverse entryChunks)
      )

processVertex ::
  VertexContexts ->
  AlgorithmSpec ->
  VertexStates ->
  MessageQueues ->
  NodeId ->
  (VertexStates, [[(NodeId, Message)]], [[LogEntry]]) ->
  (VertexStates, [[(NodeId, Message)]], [[LogEntry]])
processVertex contexts spec currentStates queues nodeId (accStates, outgoing, entries) =
  let state = Map.findWithDefault initialVertexState nodeId currentStates
      messages = Map.findWithDefault [] nodeId queues
      vtx = contexts Map.! nodeId
      VertexStepResult {vsrState = newState, vsrOutgoing = sent, vsrLogs = logs} =
        specVertexUpdate spec vtx state messages
   in ( Map.insert nodeId newState accStates,
        sent : outgoing,
        logs : entries
      )

activeVertices :: MessageQueues -> [NodeId]
activeVertices =
  Map.keys . Map.filter (not . null)

enqueueAll :: MessageQueues -> [(NodeId, Message)] -> MessageQueues
enqueueAll =
  foldr
    ( \(nodeId, message) queues ->
        Map.insertWith (++) nodeId [message] queues
    )
