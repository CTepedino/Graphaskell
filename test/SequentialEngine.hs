module SequentialEngine
  ( runPregelSequential,
    processActiveVertices,
  )
where

import Algorithm.Log (MessageLog)
import Algorithm.Types (AlgorithmSpec (..))
import Control.Monad.State.Strict (StateT (..), evalStateT, gets, put)
import qualified Data.Map.Strict as Map
import Graph.Types (NodeId)
import Graph.VertexContext (VertexContexts, buildVertexContexts)
import Pregel.Error (PregelError (..))
import Pregel.Loop (finalizeRun, loopRunner)
import Pregel.Superstep
  ( SuperstepResult (..),
    initialVertexStates,
    mergeSuperstepOutcomes,
    processVertex,
  )
import Pregel.Types

type SequentialM msg a = StateT (MessageQueues msg) (Either PregelError) a

activeVerticesWithMessages :: MessageQueues msg -> [NodeId]
activeVerticesWithMessages =
  Map.keys . Map.filter (not . null)

enqueueMessages :: MessageQueues msg -> [(NodeId, msg)] -> MessageQueues msg
enqueueMessages =
  foldr
    ( \(nodeId, message) queues ->
        Map.insertWith (++) nodeId [message] queues
    )

processActiveVertices ::
  (MessageLog msg log, Eq state) =>
  Bool ->
  AlgorithmSpec state msg log ->
  VertexContexts ->
  VertexStates state ->
  (NodeId -> [msg]) ->
  [NodeId] ->
  Either PregelError (SuperstepResult state msg log)
processActiveVertices tracing spec contexts states messageFor actives =
  case mapM (\nodeId -> processVertex tracing spec contexts states nodeId (messageFor nodeId)) actives of
    Left err ->
      Left err
    Right outcomes ->
      Right (mergeSuperstepOutcomes states outcomes)

runPregelSequential ::
  (MessageLog msg log, Eq state) =>
  RunConfig ->
  AlgorithmSpec state msg log ->
  Either PregelError (PregelRun log)
runPregelSequential cfg spec = do
  let graph = rcGraph cfg
      contexts = buildVertexContexts graph
      initialStates = initialVertexStates spec cfg graph
      initialQueues = enqueueMessages Map.empty (specBootstrap spec cfg)
  runResult <-
    evalStateT
      ( loopRunner
          cfg
          (gets activeVerticesWithMessages)
          (runSequentialSuperstep cfg spec contexts)
          ( \outgoing -> do
              put (enqueueMessages Map.empty outgoing)
              pure (Right ())
          )
          0
          initialStates
      )
      initialQueues
  runResult >>= finalizeRun cfg spec

runSequentialSuperstep ::
  (MessageLog msg log, Eq state) =>
  RunConfig ->
  AlgorithmSpec state msg log ->
  VertexContexts ->
  Int ->
  VertexStates state ->
  [NodeId] ->
  SequentialM msg (Either PregelError (SuperstepResult state msg log))
runSequentialSuperstep cfg spec contexts _step states actives = do
  queues <- gets id
  let messageFor nodeId =
        Map.findWithDefault [] nodeId queues
  pure $
    processActiveVertices (rcTrace cfg) spec contexts states messageFor actives
