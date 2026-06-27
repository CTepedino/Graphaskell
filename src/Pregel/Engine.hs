module Pregel.Engine
  ( mkRunConfig,
    runPregel,
  )
where

import Algorithm.Types (AlgorithmSpec (..))
import Control.Concurrent.STM (atomically)
import qualified Data.Map.Strict as Map
import Graph.Types
import Graph.VertexContext (VertexContexts, buildVertexContexts)
import Pregel.Env
  ( PregelEnv,
    activeVerticesSTM,
    deliverAll,
    flushVertexQueue,
    initEnv,
  )
import Pregel.Error (PregelError)
import Pregel.Pool (runPool)
import Pregel.Superstep
  ( SuperstepResult (..),
    initialVertexStates,
    mkSuperstepLog,
    processActiveVertices,
  )
import Pregel.Types

mkRunConfig ::
  Graph ->
  NodeId ->
  Maybe NodeId ->
  Int ->
  AlgorithmSpec ->
  RunConfig
mkRunConfig graph source target threads spec =
  RunConfig
    { rcGraph = graph,
      rcSource = source,
      rcTarget = target,
      rcThreads = threads,
      rcMaxSteps = specMaxSupersteps spec (nodeCount graph)
    }

runPregel :: RunConfig -> AlgorithmSpec -> IO (Either PregelError PregelRun)
runPregel = runConcurrent

runConcurrent :: RunConfig -> AlgorithmSpec -> IO (Either PregelError PregelRun)
runConcurrent cfg spec = do
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
      pure (fmap (buildRun cfg spec) runResult)
  where
    buildRun cfg' spec' (finalStates, logs, steps, maxStepsReached) =
      PregelRun
        { prSupersteps = steps,
          prLogs = logs,
          prResult = specExtractResult spec' finalStates cfg',
          prMaxStepsReached = maxStepsReached
        }

loopConcurrent ::
  RunConfig ->
  AlgorithmSpec ->
  VertexContexts ->
  Int ->
  VertexStates ->
  PregelEnv ->
  IO (Either PregelError (VertexStates, [SuperstepLog], Int, Bool))
loopConcurrent cfg spec contexts step states env
  | step >= rcMaxSteps cfg =
      pure (Right (states, [], step, True))
  | otherwise = do
      actives <- activeVerticesSTM env
      if null actives
        then pure (Right (states, [], step, False))
        else do
          fetchResults <-
            runPool
              (rcThreads cfg)
              [ atomically (flushVertexQueue nodeId env)
                | nodeId <- actives
              ]
          case sequence fetchResults of
            Left err ->
              pure (Left err)
            Right messagesList -> do
              let messageMap = Map.fromList (zip actives messagesList)
                  messageFor nodeId =
                    Map.findWithDefault [] nodeId messageMap
              case processActiveVertices spec contexts states messageFor actives of
                Left err ->
                  pure (Left err)
                Right superstepResult -> do
                  let logEntry =
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
