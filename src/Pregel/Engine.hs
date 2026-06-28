module Pregel.Engine
  ( runPregel,
  )
where

import Algorithm.Log (MessageLog)
import Algorithm.Types (AlgorithmSpec (..))
import Control.Concurrent.STM (atomically)
import Graph.Types (NodeId)
import Graph.VertexContext (VertexContexts, buildVertexContexts)
import Pregel.Env
  ( PregelEnv,
    activeVerticesSTM,
    deliverAll,
    flushVertexQueue,
    initEnv,
  )
import Pregel.Error (PregelError (..))
import Pregel.Loop (finalizeRun, loopRunner)
import Pregel.Pool (runPool)
import Pregel.Superstep
  ( SuperstepResult (..),
    initialVertexStates,
    mergeSuperstepOutcomes,
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
        loopRunner
          cfg
          (activeVerticesSTM env)
          (runConcurrentSuperstep cfg spec contexts env)
          (deliverAll env)
          0
          initialStates
      pure (runResult >>= finalizeRun cfg spec)

runConcurrentSuperstep ::
  MessageLog msg log =>
  RunConfig ->
  AlgorithmSpec state msg log ->
  VertexContexts ->
  PregelEnv msg ->
  Int ->
  VertexStates state ->
  [NodeId] ->
  IO (Either PregelError (SuperstepResult state msg log))
runConcurrentSuperstep cfg spec contexts env _step states actives = do
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
  pure $
    case sequence workerResults of
      Left err ->
        Left err
      Right outcomes ->
        Right (mergeSuperstepOutcomes states outcomes)
