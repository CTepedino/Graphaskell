module SequentialEngine
  ( runSequential,
    runSomeSequential,
  )
where

import Algorithm.Types (AlgorithmSpec (..), SomeAlgorithmSpec (..))
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

runSomeSequential :: RunConfig -> SomeAlgorithmSpec -> Either PregelError SomePregelRun
runSomeSequential cfg (SomeAlgorithmSpec spec) =
  fmap SomePregelRun (runSequential cfg spec)

runSequential :: RunConfig -> AlgorithmSpec state msg -> Either PregelError (PregelRun msg)
runSequential cfg spec = do
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
  VertexContexts ->
  AlgorithmSpec state msg ->
  RunConfig ->
  Int ->
  VertexStates state ->
  MessageQueues msg ->
  Either PregelError (VertexStates state, [SuperstepLog msg], Int, Bool)
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
