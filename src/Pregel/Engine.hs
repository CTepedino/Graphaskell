module Pregel.Engine
  ( PregelRun (..),
    mkRunConfig,
    runPregel,
    runSequential,
  )
where

import Algorithm.Types (AlgorithmSpec (..))
import Control.Concurrent.STM (atomically)
import qualified Data.Map.Strict as Map
import Graph.Parser (GraphFile (..))
import Graph.Types
import Pregel.Env
import Pregel.Pool (runPool)
import Pregel.Types

data PregelRun = PregelRun
  { prSupersteps :: Int,
    prLogs :: [SuperstepLog],
    prResult :: Result,
    prFinalStates :: VertexStates
  }
  deriving (Eq, Show)

data VertexWorkerResult = VertexWorkerResult
  { vwrNodeId :: NodeId,
    vwrState :: VertexState,
    vwrOutgoing :: [(NodeId, Message)],
    vwrLogs :: [LogEntry]
  }

mkRunConfig :: GraphFile -> Int -> RunConfig
mkRunConfig gf threads =
  RunConfig
    { rcGraph = gfGraph gf,
      rcSource = gfSource gf,
      rcTarget = gfTarget gf,
      rcThreads = threads,
      rcAlgorithm = gfAlgorithm gf,
      rcMaxSteps = maxSteps (gfGraph gf)
    }
  where
    maxSteps graph =
      let n = nodeCount graph
       in max 1 (n * n)

runPregel :: RunConfig -> AlgorithmSpec -> IO PregelRun
runPregel cfg spec = runConcurrent cfg spec

runSequential :: RunConfig -> AlgorithmSpec -> PregelRun
runSequential cfg spec =
  let graph = rcGraph cfg
      nodes = graphNodes graph
      initialStates =
        Map.fromList
          [ (nodeId, specInitState spec nodeId cfg)
            | nodeId <- nodes
          ]
      initialQueues = enqueueAll Map.empty (specBootstrap spec cfg)
      (finalStates, logs, steps) =
        loop graph spec cfg 0 initialStates initialQueues
   in PregelRun
        { prSupersteps = steps,
          prLogs = logs,
          prResult = specExtractResult spec finalStates cfg,
          prFinalStates = finalStates
        }

runConcurrent :: RunConfig -> AlgorithmSpec -> IO PregelRun
runConcurrent cfg spec = do
  env <- initEnv (rcGraph cfg)
  let initialStates =
        Map.fromList
          [ (nodeId, specInitState spec nodeId cfg)
            | nodeId <- graphNodes (rcGraph cfg)
          ]
  deliverAll env (specBootstrap spec cfg)
  (finalStates, logs, steps) <-
    loopConcurrent cfg spec 0 initialStates env
  pure
    PregelRun
      { prSupersteps = steps,
        prLogs = logs,
        prResult = specExtractResult spec finalStates cfg,
        prFinalStates = finalStates
      }

loopConcurrent ::
  RunConfig ->
  AlgorithmSpec ->
  Int ->
  VertexStates ->
  PregelEnv ->
  IO (VertexStates, [SuperstepLog], Int)
loopConcurrent cfg spec step states env
  | step >= rcMaxSteps cfg =
      pure (states, [], step)
  | otherwise = do
      actives <- activeVerticesSTM env
      if null actives
        then pure (states, [], step)
        else do
          results <-
            runPool
              (rcThreads cfg)
              [ processVertexConcurrent cfg spec env states nodeId
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
            then pure (newStates, [logEntry], step + 1)
            else do
              deliverAll env outgoing
              (finalStates, restLogs, finalStep) <-
                loopConcurrent cfg spec (step + 1) newStates env
              pure (finalStates, logEntry : restLogs, finalStep)

processVertexConcurrent ::
  RunConfig ->
  AlgorithmSpec ->
  PregelEnv ->
  VertexStates ->
  NodeId ->
  IO VertexWorkerResult
processVertexConcurrent cfg spec env states nodeId = do
  messages <-
    atomically $
      flushQueue (peQueues env Map.! nodeId)
  let state = Map.findWithDefault initialVertexState nodeId states
      VertexStepResult {vsrState = newState, vsrOutgoing = sent, vsrLogs = logs} =
        specVertexUpdate spec (rcGraph cfg) states nodeId state messages
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
mergeWorkerResults states results =
  foldr
    ( \result acc ->
        Map.insert (vwrNodeId result) (vwrState result) acc
    )
    states
    results

-- | Motor secuencial (referencia y pruebas)

loop ::
  Graph ->
  AlgorithmSpec ->
  RunConfig ->
  Int ->
  VertexStates ->
  MessageQueues ->
  (VertexStates, [SuperstepLog], Int)
loop graph spec cfg step states queues
  | step >= rcMaxSteps cfg =
      (states, [], step)
  | null (activeVertices queues) =
      (states, [], step)
  | otherwise =
      let actives = activeVertices queues
          (newStates, outgoing, entries, _) =
            processActive graph spec actives states queues
          logEntry =
            SuperstepLog
              { sslStep = step,
                sslActiveVertices = length actives,
                sslMessagesSent = length outgoing,
                sslEntries = entries
              }
       in if null outgoing
            then (newStates, [logEntry], step + 1)
            else
              let nextQueues = enqueueAll Map.empty outgoing
                  (finalStates, restLogs, finalStep) =
                    loop graph spec cfg (step + 1) newStates nextQueues
               in (finalStates, logEntry : restLogs, finalStep)

processActive ::
  Graph ->
  AlgorithmSpec ->
  [NodeId] ->
  VertexStates ->
  MessageQueues ->
  (VertexStates, [(NodeId, Message)], [LogEntry], Bool)
processActive graph spec actives states queues =
  foldr
    (processVertex graph spec states queues)
    (states, [], [], False)
    actives

processVertex ::
  Graph ->
  AlgorithmSpec ->
  VertexStates ->
  MessageQueues ->
  NodeId ->
  (VertexStates, [(NodeId, Message)], [LogEntry], Bool) ->
  (VertexStates, [(NodeId, Message)], [LogEntry], Bool)
processVertex graph spec _states queues nodeId (currentStates, outgoing, entries, _) =
  let state = Map.findWithDefault initialVertexState nodeId currentStates
      messages = Map.findWithDefault [] nodeId queues
      VertexStepResult {vsrState = newState, vsrOutgoing = sent, vsrLogs = logs} =
        specVertexUpdate spec graph currentStates nodeId state messages
   in ( Map.insert nodeId newState currentStates,
        outgoing ++ sent,
        entries ++ logs,
        True
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
