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
  putStrLn $ "  Graph:      " ++ optGraphPath opts
  putStrLn $ "  Source:     " ++ show (optSource opts)
  putStrLn $
    "  Target:     "
      ++ maybe "—" show (optTarget opts)
  putStrLn $ "  Algorithm:  " ++ show (optAlgorithm opts)
  putStrLn $
    "  Threads:    "
      ++ show (optThreads opts)
      ++ " / "
      ++ show (optMaxCapabilities opts)
      ++ " capabilities"
  putStrLn $
    "  Mode:       "
      ++ if optSequential opts then "sequential" else "concurrent (async + STM)"
  putStrLn $
    "  Verbose:    "
      ++ if optVerbose opts then "yes" else "no"
  putStrLn ""

  graphResult <- loadGraphFile (optGraphPath opts)
  case graphResult of
    Left err ->
      die $ "Error loading graph: " ++ displayLoadGraphError err
    Right graph -> do
      case validateRunNodes graph (optSource opts) (optTarget opts) of
        Left parseError ->
          die $ "Error in source/target: " ++ displayParseError parseError
        Right () -> do
          putStrLn "Graph loaded:"
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
                then "Pregel execution (sequential):"
                else "Pregel execution (async + STM):"
            )
          putStrLn ""
          putStrLn (describeRun (optVerbose opts) pregelRun)
