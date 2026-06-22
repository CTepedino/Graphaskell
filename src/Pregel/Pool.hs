module Pregel.Pool
  ( runPool,
  )
where

import Control.Concurrent.Async (async, wait)

-- | Ejecuta acciones IO en lotes de tamaño fijo (pool de workers).
runPool :: Int -> [IO a] -> IO [a]
runPool _ [] = pure []
runPool maxWorkers tasks =
  let (batch, rest) = splitAt maxWorkers tasks
   in do
        workers <- mapM async batch
        results <- mapM wait workers
        restResults <- runPool maxWorkers rest
        pure (results ++ restResults)
