module Main where

import Algorithm.Spec (resolveAlgorithm, validatePathTarget)
import AppError (AppError (..), displayAppError)
import Cli.Options (Options (..), parseOptions)
import Data.Bifunctor (first)
import Graph.Parser
  ( describeGraph,
    loadGraphFile,
    validateRunNodes,
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
  result <- execute opts
  case result of
    Left err -> die (displayAppError err)
    Right outputLines -> mapM_ putStrLn outputLines

execute :: Options -> IO (Either AppError [String])
execute opts =
  case first AppAlgorithm (validatePathTarget (optAlgorithm opts) (optTarget opts)) of
    Left err -> pure (Left err)
    Right () ->
      loadGraphFile (optGraphPath opts) >>= \graphResult ->
        case graphResult of
          Left loadErr -> pure (Left (AppLoad loadErr))
          Right graph ->
            case first AppParse (validateRunNodes graph (optSource opts) (optTarget opts)) of
              Left err -> pure (Left err)
              Right () ->
                case first AppAlgorithm (resolveAlgorithm graph (optAlgorithm opts)) of
                  Left err -> pure (Left err)
                  Right spec -> do
                    let cfg =
                          mkRunConfig
                            graph
                            (optSource opts)
                            (optTarget opts)
                            (optThreads opts)
                    pregelRun <- runPregel cfg spec
                    pure
                      ( Right
                          ( configBanner opts
                              ++ [ "Graph loaded:",
                                   "",
                                   describeGraph graph,
                                   "",
                                   "Pregel execution (async + STM):",
                                   "",
                                   describeRun (optVerbose opts) pregelRun
                                 ]
                          )
                      )

configBanner :: Options -> [String]
configBanner opts =
  [ "Graphaskell",
    "",
    "  Graph:      " ++ optGraphPath opts,
    "  Source:     " ++ show (optSource opts),
    "  Target:     " ++ maybe "—" show (optTarget opts),
    "  Algorithm:  " ++ show (optAlgorithm opts),
    "  Threads:    "
      ++ show (optThreads opts)
      ++ " / "
      ++ show (optMaxCapabilities opts)
      ++ " capabilities",
    "  Verbose:    "
      ++ if optVerbose opts then "yes" else "no",
    ""
  ]
