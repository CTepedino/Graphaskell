module SequentialEngine
  ( runPregelSequential,
  )
where

import Algorithm.Types (AlgorithmSpec (..))
import Algorithm.Log (MessageLog)
import qualified Data.Map.Strict as Map
import Graph.VertexContext (VertexContexts, buildVertexContexts)
import Pregel.Error (PregelError (..))
import Pregel.Superstep
  ( SuperstepResult (..),
    activeVerticesWithMessages,
    enqueueMessages,
    initialVertexStates,
    mkSuperstepLog,
    processActiveVertices,
  )
import Pregel.Types

runPregelSequential ::
  MessageLog msg log =>
  RunConfig ->
  AlgorithmSpec state msg log ->
  Either PregelError (PregelRun log)
runPregelSequential cfg spec = do
  let graph = rcGraph cfg
      contexts = buildVertexContexts graph
      initialStates = initialVertexStates spec cfg graph
      initialQueues = enqueueMessages Map.empty (specBootstrap spec cfg)
  (finalStates, logs, steps, maxStepsReached) <-
    loop contexts spec cfg 0 initialStates initialQueues
  case specExtractResult spec finalStates cfg of
    Left algoErr ->
      Left (ResultExtraction algoErr)
    Right result ->
      pure
        PregelRun
          { prSupersteps = steps,
            prLogs = logs,
            prResult = result,
            prMaxStepsReached = maxStepsReached
          }

loop ::
  MessageLog msg log =>
  VertexContexts ->
  AlgorithmSpec state msg log ->
  RunConfig ->
  Int ->
  VertexStates state ->
  MessageQueues msg ->
  Either PregelError (VertexStates state, [SuperstepLog log], Int, Bool)
loop contexts spec cfg step states queues
  | step >= rcMaxSteps cfg =
      pure (states, [], step, True)
  | null (activeVerticesWithMessages queues) =
      pure (states, [], step, False)
  | otherwise = do
      let actives = activeVerticesWithMessages queues
          messageFor nodeId =
            Map.findWithDefault [] nodeId queues
      SuperstepResult {ssNewStates = newStates, ssOutgoing = outgoing, ssEntries = entries} <-
        processActiveVertices (rcTrace cfg) spec contexts states messageFor actives
      let logEntry = mkSuperstepLog step actives outgoing entries
      if null outgoing
        then pure (newStates, [logEntry], step + 1, False)
        else do
          (finalStates, restLogs, finalStep, maxStepsReached) <-
            loop contexts spec cfg (step + 1) newStates (enqueueMessages Map.empty outgoing)
          pure (finalStates, logEntry : restLogs, finalStep, maxStepsReached)
