module Main where

import Algorithm.Error (displayAlgorithmError)
import Algorithm.Spec (resolveAlgorithm)
import Cli.Options (Options (..), parseOptions)
import Graph.Parser
  ( GraphFile (..),
    describeGraphFile,
    displayLoadGraphError,
    loadGraphFile,
  )
import Output.Trace (describeRun)
import Pregel.Engine (mkRunConfig, runPregel)
import System.Exit (die)

main :: IO ()
main = do
  opts <- parseOptions
  run opts

run :: Options -> IO ()
run opts = do
  putStrLn "Graphaskell"
  putStrLn ""
  putStrLn $ "  Grafo:        " ++ optGraphPath opts
  putStrLn $
    "  Threads:      "
      ++ show (optThreads opts)
      ++ " / "
      ++ show (optMaxCapabilities opts)
      ++ " capacidades"
  putStrLn $
    "  Verbose:      "
      ++ if optVerbose opts then "si" else "no"
  putStrLn ""

  graphResult <- loadGraphFile (optGraphPath opts)
  case graphResult of
    Left err ->
      die $ "Error al cargar el grafo: " ++ displayLoadGraphError err
    Right graphFile -> do
      putStrLn "Grafo cargado:"
      putStrLn ""
      putStrLn (describeGraphFile graphFile)
      putStrLn ""

      spec <-
        either
          (\algorithmError -> die (displayAlgorithmError algorithmError))
          pure
          (resolveAlgorithm (gfGraph graphFile) (gfAlgorithm graphFile))
      let cfg = mkRunConfig graphFile (optThreads opts)
      pregelRun <- runPregel cfg spec

      putStrLn "Ejecucion Pregel (async + STM):"
      putStrLn ""
      putStrLn (describeRun (optVerbose opts) pregelRun)
