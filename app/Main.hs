module Main where

import Control.Concurrent.Async (async, wait)
import Control.Concurrent.STM
import Control.Monad (replicateM, replicateM_)

main :: IO ()
main = do
  putStrLn "Graphaskell — hello world (async + STM)"
  putStrLn ""

  counter <- atomically $ newTVar (0 :: Int)

  workers <- replicateM 4 $
    async $ replicateM_ 1_000 $
      atomically $ modifyTVar' counter (+ 1)

  mapM_ wait workers

  total <- atomically $ readTVar counter
  putStrLn $ "Contador atomico: " ++ show total ++ " (esperado: 4000)"
  putStrLn ""
  putStrLn "Listo. El stack async + STM compila y corre."
