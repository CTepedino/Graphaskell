module Pregel.Engine
  ( runPathPregel,
    runGlobalPregel,
  )
where

import Algorithm.Types
  ( AlgorithmSpec (..),
    GlobalAlgorithmSpec,
    PathAlgorithmSpec,
    PathLog,
    globalRunSpec,
    pathRunSpec,
  )
import Algorithm.Log (MessageLog)
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
import Pregel.Error (PregelError (..))
import Pregel.Pool (runPool)
import Pregel.Superstep
  ( SuperstepResult (..),
    initialVertexStates,
    mkSuperstepLog,
    processActiveVertices,
  )
import Pregel.Types

runPathPregel ::
  PathRunConfig ->
  PathAlgorithmSpec ->
  IO (Either PregelError (PregelRun PathLog))
runPathPregel prc pathSpec =
  runConcurrent (pathRunConfigToRunConfig prc) (pathRunSpec pathSpec prc)

runGlobalPregel ::
  MessageLog msg log =>
  RunConfig ->
  GlobalAlgorithmSpec state msg log ->
  IO (Either PregelError (PregelRun log))
runGlobalPregel cfg globalSpec =
  runConcurrent cfg (globalRunSpec globalSpec cfg)

runConcurrent ::
  MessageLog msg log =>
  RunConfig ->
  AlgorithmSpec state msg log ->
  IO (Either PregelError (PregelRun log))
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
      pure (runResult >>= buildRun spec)
  where
    buildRun spec' (finalStates, logs, steps, maxStepsReached) =
      case specExtractResult spec' finalStates of
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
              case processActiveVertices (rcTrace cfg) spec contexts states messageFor actives of
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
