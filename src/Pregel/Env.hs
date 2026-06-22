module Pregel.Env
  ( PregelEnv (..),
    initEnv,
    flushQueue,
    deliverAll,
    activeVerticesSTM,
  )
where

import Control.Concurrent.STM
import Control.Monad (foldM, forM_)
import Data.Foldable (toList)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import Graph.Types
import Pregel.Types

data PregelEnv = PregelEnv
  { peQueues :: Map.Map NodeId (TQueue Message)
  }

initEnv :: Graph -> IO PregelEnv
initEnv graph = do
  queuePairs <-
    mapM
      ( \nodeId -> do
          queue <- newTQueueIO
          pure (nodeId, queue)
      )
      (graphNodes graph)
  pure PregelEnv {peQueues = Map.fromList queuePairs}

flushQueue :: TQueue Message -> STM [Message]
flushQueue queue = go Seq.empty
  where
    go acc = do
      empty <- isEmptyTQueue queue
      if empty
        then pure (toList acc)
        else do
          message <- readTQueue queue
          go (acc Seq.|> message)

deliverAll :: PregelEnv -> [(NodeId, Message)] -> IO ()
deliverAll env outgoing =
  atomically $
    forM_ outgoing $
      \(nodeId, message) ->
        writeTQueue (peQueues env Map.! nodeId) message

activeVerticesSTM :: PregelEnv -> IO [NodeId]
activeVerticesSTM env =
  atomically $
    foldM
      ( \acc (nodeId, queue) -> do
          empty <- isEmptyTQueue queue
          pure (if empty then acc else nodeId : acc)
      )
      []
      (Map.toList (peQueues env))
