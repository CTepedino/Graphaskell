module Pregel.Engine
  ( runPregel,
  )
where

import Algorithm.Log (MessageLog)
import Algorithm.Types (AlgorithmSpec (..))
import Control.Concurrent.STM (atomically)
import Graph.VertexContext (VertexContexts, buildVertexContexts)
import Pregel.Env
  ( PregelEnv,
    activeVerticesSTM,
    deliverAll,
    flushVertexQueue,
    initEnv,
  )
import Pregel.Error (PregelError (..))
import Pregel.Pool (runPool)
import Pregel.Superstep
  ( SuperstepResult (..),
    initialVertexStates,
    mergeSuperstepOutcomes,
    mkSuperstepLog,
    processVertex,
  )
import Pregel.Types

runPregel ::
  MessageLog msg log =>
  RunConfig ->
  AlgorithmSpec state msg log ->
  IO (Either PregelError (PregelRun log))
runPregel cfg spec = do
  let graph = rcGraph cfg
      contexts = buildVertexContexts graph
  env <- initEnv graph
  let initialStates = initialVertexStates spec cfg graph
  deliverResult <- deliverAll env (specBootstrap spec cfg)
  case deliverResult of
    Left err ->
      pure (Left err)
    Right () -> do
      runResult <-
        loopConcurrent cfg spec contexts 0 initialStates env
      pure (runResult >>= buildRun cfg spec)
  where
    buildRun cfg' spec' (finalStates, logs, steps, maxStepsReached) =
      case specExtractResult spec' finalStates cfg' of
        Left algoErr ->
          Left (ResultExtraction algoErr)
        Right result ->
          Right
            PregelRun
              { prSupersteps = steps,
                prLogs = logs,
                prResult = result,
                prMaxStepsReached = maxStepsReached
              }

loopConcurrent ::
  MessageLog msg log =>
  RunConfig ->
  AlgorithmSpec state msg log ->
  VertexContexts ->
  Int ->
  VertexStates state ->
  PregelEnv msg ->
  IO (Either PregelError (VertexStates state, [SuperstepLog log], Int, Bool))
loopConcurrent cfg spec contexts step states env
  | step >= rcMaxSteps cfg =
      pure (Right (states, [], step, True))
  | otherwise = do
      actives <- activeVerticesSTM env
      if null actives
        then pure (Right (states, [], step, False))
        else do
          let workers = min (rcThreads cfg) (length actives)
              tracing = rcTrace cfg
          workerResults <-
            runPool
              workers
              [ do
                  fetchResult <- atomically (flushVertexQueue nodeId env)
                  case fetchResult of
                    Left err ->
                      pure (Left err)
                    Right messages ->
                      pure (processVertex tracing spec contexts states nodeId messages)
              | nodeId <- actives
              ]
          case sequence workerResults of
            Left err ->
              pure (Left err)
            Right outcomes -> do
              let superstepResult = mergeSuperstepOutcomes states outcomes
                  logEntry =
                    mkSuperstepLog
                      step
                      actives
                      (ssOutgoing superstepResult)
                      (ssEntries superstepResult)
              if null (ssOutgoing superstepResult)
                then
                  pure
                    ( Right
                        ( ssNewStates superstepResult,
                          [logEntry],
                          step + 1,
                          False
                        )
                    )
                else do
                  deliverResult <-
                    deliverAll env (ssOutgoing superstepResult)
                  case deliverResult of
                    Left err ->
                      pure (Left err)
                    Right () -> do
                      loopResult <-
                        loopConcurrent
                          cfg
                          spec
                          contexts
                          (step + 1)
                          (ssNewStates superstepResult)
                          env
                      pure
                        ( fmap
                            ( \(finalStates, restLogs, finalStep, maxStepsReached) ->
                                (finalStates, logEntry : restLogs, finalStep, maxStepsReached)
                            )
                            loopResult
                        )