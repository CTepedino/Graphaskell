module Pregel.Env
  ( PregelEnv (..),
    initEnv,
    flushQueue,
    flushVertexQueue,
    deliverAll,
    activeVerticesSTM,
  )
where

import Control.Concurrent.STM
import Control.Monad (foldM, forM)
import Data.Foldable (toList)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import Graph.Types
import Pregel.Error (PregelError (..))
import Pregel.Types

data PregelEnv msg = PregelEnv
  { peQueues :: Map.Map NodeId (TQueue msg)
  }

initEnv :: Graph -> IO (PregelEnv msg)
initEnv graph = do
  queuePairs <-
    mapM
      ( \nodeId -> do
          queue <- newTQueueIO
          pure (nodeId, queue)
      )
      (graphNodes graph)
  pure PregelEnv {peQueues = Map.fromList queuePairs}

flushQueue :: TQueue msg -> STM [msg]
flushQueue queue = go Seq.empty
  where
    go acc = do
      empty <- isEmptyTQueue queue
      if empty
        then pure (toList acc)
        else do
          message <- readTQueue queue
          go (acc Seq.|> message)

lookupQueue :: NodeId -> PregelEnv msg -> Maybe (TQueue msg)
lookupQueue nodeId env =
  Map.lookup nodeId (peQueues env)

flushVertexQueue :: NodeId -> PregelEnv msg -> STM (Either PregelError [msg])
flushVertexQueue nodeId env =
  case lookupQueue nodeId env of
    Just queue ->
      fmap Right (flushQueue queue)
    Nothing ->
      pure (Left (MissingMessageQueue nodeId))

writeQueue :: NodeId -> msg -> PregelEnv msg -> STM (Either PregelError ())
writeQueue nodeId message env =
  case lookupQueue nodeId env of
    Just queue -> do
      writeTQueue queue message
      pure (Right ())
    Nothing ->
      pure (Left (MissingMessageQueue nodeId))

deliverAll :: PregelEnv msg -> [(NodeId, msg)] -> IO (Either PregelError ())
deliverAll env outgoing =
  atomically $
    foldM
      ( \acc (nodeId, message) ->
          case acc of
            Left err -> pure (Left err)
            Right () -> writeQueue nodeId message env
      )
      (Right ())
      outgoing

activeVerticesSTM :: PregelEnv msg -> IO [NodeId]
activeVerticesSTM env =
  atomically $ do
    flags <-
      forM (Map.toList (peQueues env)) $ \(nodeId, queue) -> do
        empty <- isEmptyTQueue queue
        pure (nodeId, not empty)
    pure [nodeId | (nodeId, True) <- flags]
