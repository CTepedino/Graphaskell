module Pregel.Pool
  ( runPool,
  )
where

import Control.Concurrent.Async (async, wait)

-- | Run IO actions with at most @maxWorkers@ in flight at once.
-- Batches tasks sequentially: this is a simple worker cap, not work stealing.
runPool :: Int -> [IO a] -> IO [a]
runPool _ [] = pure []
runPool maxWorkers tasks =
  let (batch, rest) = splitAt maxWorkers tasks
   in do
        workers <- mapM async batch
        results <- mapM wait workers
        restResults <- runPool maxWorkers rest
        pure (results ++ restResults)
