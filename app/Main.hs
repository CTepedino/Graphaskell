module Main where

import Cli.Options (Options (..), parseOptions)
import Control.Concurrent.Async (async, wait)
import Control.Concurrent.STM
import Control.Monad (replicateM, replicateM_)

main :: IO ()
main = do
  opts <- parseOptions
  runDemo opts

runDemo :: Options -> IO ()
runDemo opts = do
  putStrLn "Graphaskell"
  putStrLn ""
  putStrLn $ "  Grafo:        " ++ optGraphPath opts
  putStrLn $
    "  Threads:      "
      ++ show (optThreads opts)
      ++ " / "
      ++ show (optMaxCapabilities opts)
      ++ " capacidades"
  putStrLn ""

  counter <- atomically $ newTVar (0 :: Int)

  workers <-
    replicateM (optThreads opts) $
      async $
        replicateM_ 1_000 $
          atomically $
            modifyTVar' counter (+ 1)

  mapM_ wait workers

  total <- atomically $ readTVar counter
  let expected = optThreads opts * 1_000
  putStrLn $ "Contador atomico: " ++ show total ++ " (esperado: " ++ show expected ++ ")"
