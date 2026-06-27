module Pregel.Engine
  ( PregelRun (..),
    mkRunConfig,
    runPregel,
  )
where

import Algorithm.Types (AlgorithmSpec (..))
import Control.Concurrent.STM (atomically)
import qualified Data.Map.Strict as Map
import Graph.Types
import Graph.VertexContext (VertexContexts, buildVertexContexts)
import Pregel.Env
import Pregel.Pool (runPool)
import Pregel.Types

data PregelRun = PregelRun
  { prSupersteps :: Int,
    prLogs :: [SuperstepLog],
    prResult :: Result,
    prMaxStepsReached :: Bool
  }
  deriving (Eq, Show)

data VertexWorkerResult = VertexWorkerResult
  { vwrNodeId :: NodeId,
    vwrState :: VertexState,
    vwrOutgoing :: [(NodeId, Message)],
    vwrLogs :: [LogEntry]
  }

mkRunConfig ::
  Graph ->
  NodeId ->
  Maybe NodeId ->
  Int ->
  RunConfig
mkRunConfig graph source target threads =
  RunConfig
    { rcGraph = graph,
      rcSource = source,
      rcTarget = target,
      rcThreads = threads,
      rcMaxSteps = maxSteps graph
    }
  where
    maxSteps g =
      let n = nodeCount g
       in max 1 (n * n)

runPregel :: RunConfig -> AlgorithmSpec -> IO PregelRun
runPregel = runConcurrent

runConcurrent :: RunConfig -> AlgorithmSpec -> IO PregelRun
runConcurrent cfg spec = do
  let graph = rcGraph cfg
      contexts = buildVertexContexts graph
  env <- initEnv graph
  let initialStates =
        Map.fromList
          [ (nodeId, specInitState spec nodeId cfg)
            | nodeId <- graphNodes graph
          ]
  deliverAll env (specBootstrap spec cfg)
  (finalStates, logs, steps, maxStepsReached) <-
    loopConcurrent cfg spec contexts 0 initialStates env
  pure
    PregelRun
      { prSupersteps = steps,
        prLogs = logs,
        prResult = specExtractResult spec finalStates cfg,
        prMaxStepsReached = maxStepsReached
      }

loopConcurrent ::
  RunConfig ->
  AlgorithmSpec ->
  VertexContexts ->
  Int ->
  VertexStates ->
  PregelEnv ->
  IO (VertexStates, [SuperstepLog], Int, Bool)
loopConcurrent cfg spec contexts step states env
  | step >= rcMaxSteps cfg =
      pure (states, [], step, True)
  | otherwise = do
      actives <- activeVerticesSTM env
      if null actives
        then pure (states, [], step, False)
        else do
          results <-
            runPool
              (rcThreads cfg)
              [ processVertexConcurrent cfg spec contexts env states nodeId
                | nodeId <- actives
              ]
          let newStates = mergeWorkerResults states results
              outgoing = concatMap vwrOutgoing results
              entries = concatMap vwrLogs results
              logEntry =
                SuperstepLog
                  { sslStep = step,
                    sslActiveVertices = length actives,
                    sslMessagesSent = length outgoing,
                    sslEntries = entries
                  }
          if null outgoing
            then pure (newStates, [logEntry], step + 1, False)
            else do
              deliverAll env outgoing
              (finalStates, restLogs, finalStep, maxStepsReached) <-
                loopConcurrent cfg spec contexts (step + 1) newStates env
              pure (finalStates, logEntry : restLogs, finalStep, maxStepsReached)

processVertexConcurrent ::
  RunConfig ->
  AlgorithmSpec ->
  VertexContexts ->
  PregelEnv ->
  VertexStates ->
  NodeId ->
  IO VertexWorkerResult
processVertexConcurrent _cfg spec contexts env states nodeId = do
  messages <-
    atomically $
      flushQueue (peQueues env Map.! nodeId)
  let state = Map.findWithDefault initialVertexState nodeId states
      vtx = contexts Map.! nodeId
      VertexStepResult {vsrState = newState, vsrOutgoing = sent, vsrLogs = logs} =
        specVertexUpdate spec vtx state messages
  pure
    VertexWorkerResult
      { vwrNodeId = nodeId,
        vwrState = newState,
        vwrOutgoing = sent,
        vwrLogs = logs
      }

mergeWorkerResults ::
  VertexStates ->
  [VertexWorkerResult] ->
  VertexStates
mergeWorkerResults = foldr
  ( \result acc ->
      Map.insert (vwrNodeId result) (vwrState result) acc
  )
