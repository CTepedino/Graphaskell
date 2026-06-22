module Pregel.Pool
  ( runPool,
  )
where

import Control.Concurrent.Async (async, wait)

-- | Run IO actions in fixed-size batches (worker pool).
runPool :: Int -> [IO a] -> IO [a]
runPool _ [] = pure []
runPool maxWorkers tasks =
  let (batch, rest) = splitAt maxWorkers tasks
   in do
        workers <- mapM async batch
        results <- mapM wait workers
        restResults <- runPool maxWorkers rest
        pure (results ++ restResults)
