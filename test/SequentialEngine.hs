module SequentialEngine
  ( runSequential,
  )
where

import Algorithm.Types (AlgorithmSpec (..))
import qualified Data.Map.Strict as Map
import Graph.VertexContext (VertexContexts, buildVertexContexts)
import Pregel.Types
import Pregel.Error (PregelError)
import Pregel.Superstep
  ( SuperstepResult (..),
    activeVerticesWithMessages,
    enqueueMessages,
    initialVertexStates,
    mkSuperstepLog,
    processActiveVertices,
  )
import Pregel.Types

runSequential :: RunConfig -> AlgorithmSpec -> Either PregelError PregelRun
runSequential cfg spec = do
  let graph = rcGraph cfg
      contexts = buildVertexContexts graph
      initialStates = initialVertexStates spec cfg graph
      initialQueues = enqueueMessages Map.empty (specBootstrap spec cfg)
  (finalStates, logs, steps, maxStepsReached) <-
    loop contexts spec cfg 0 initialStates initialQueues
  pure
    PregelRun
      { prSupersteps = steps,
        prLogs = logs,
        prResult = specExtractResult spec finalStates cfg,
        prMaxStepsReached = maxStepsReached
      }

loop ::
  VertexContexts ->
  AlgorithmSpec ->
  RunConfig ->
  Int ->
  VertexStates ->
  MessageQueues ->
  Either PregelError (VertexStates, [SuperstepLog], Int, Bool)
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
        processActiveVertices spec contexts states messageFor actives
      let logEntry = mkSuperstepLog step actives outgoing entries
      if null outgoing
        then pure (newStates, [logEntry], step + 1, False)
        else do
          (finalStates, restLogs, finalStep, maxStepsReached) <-
            loop contexts spec cfg (step + 1) newStates (enqueueMessages Map.empty outgoing)
          pure (finalStates, logEntry : restLogs, finalStep, maxStepsReached)
