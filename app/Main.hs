module Main where

import Algorithm.Error (displayAlgorithmError)
import Algorithm.Spec (resolveAlgorithm, validatePathTarget)
import Cli.Options (Options (..), parseOptions)
import Graph.ParseError (displayParseError)
import Graph.Parser
  ( describeGraph,
    displayLoadGraphError,
    loadGraphFile,
    validateRunNodes,
  )
import Output.Trace (describeRun)
import Pregel.Engine (mkRunConfig, runPregel, runSequential)
import System.Exit (die)

main :: IO ()
main = do
  opts <- parseOptions
  run opts

run :: Options -> IO ()
run opts = do
  either
    (\algorithmError -> die (displayAlgorithmError algorithmError))
    (const (pure ()))
    (validatePathTarget (optAlgorithm opts) (optTarget opts))

  putStrLn "Graphaskell"
  putStrLn ""
  putStrLn $ "  Grafo:        " ++ optGraphPath opts
  putStrLn $ "  Origen:       " ++ show (optSource opts)
  putStrLn $
    "  Destino:      "
      ++ maybe "—" show (optTarget opts)
  putStrLn $ "  Algoritmo:    " ++ show (optAlgorithm opts)
  putStrLn $
    "  Threads:      "
      ++ show (optThreads opts)
      ++ " / "
      ++ show (optMaxCapabilities opts)
      ++ " capacidades"
  putStrLn $
    "  Modo:         "
      ++ if optSequential opts then "secuencial" else "concurrente (async + STM)"
  putStrLn $
    "  Verbose:      "
      ++ if optVerbose opts then "si" else "no"
  putStrLn ""

  graphResult <- loadGraphFile (optGraphPath opts)
  case graphResult of
    Left err ->
      die $ "Error al cargar el grafo: " ++ displayLoadGraphError err
    Right graph -> do
      case validateRunNodes graph (optSource opts) (optTarget opts) of
        Left parseError ->
          die $ "Error en origen/destino: " ++ displayParseError parseError
        Right () -> do
          putStrLn "Grafo cargado:"
          putStrLn ""
          putStrLn (describeGraph graph)
          putStrLn ""

          spec <-
            either
              (\algorithmError -> die (displayAlgorithmError algorithmError))
              pure
              (resolveAlgorithm graph (optAlgorithm opts))
          let cfg =
                mkRunConfig
                  graph
                  (optSource opts)
                  (optTarget opts)
                  (optAlgorithm opts)
                  (optThreads opts)
          pregelRun <-
            if optSequential opts
              then pure $ runSequential cfg spec
              else runPregel cfg spec

          putStrLn
            ( if optSequential opts
                then "Ejecucion Pregel (secuencial):"
                else "Ejecucion Pregel (async + STM):"
            )
          putStrLn ""
          putStrLn (describeRun (optVerbose opts) pregelRun)
