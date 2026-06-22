module Main where

import Algorithm.Spec (resolveAlgorithm, validatePathTarget)
import AppError (AppError (..), displayAppError)
import Cli.Options (Options (..), parseOptions)
import Control.Monad.Except (ExceptT (..), liftEither, runExceptT)
import Control.Monad.Trans (lift)
import Data.Bifunctor (first)
import Graph.Parser
  ( describeGraph,
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
  result <- runExceptT (execute opts)
  case result of
    Left err -> die (displayAppError err)
    Right outputLines -> mapM_ putStrLn outputLines

execute :: Options -> ExceptT AppError IO [String]
execute opts = do
  liftEither $
    first AppAlgorithm $
      validatePathTarget (optAlgorithm opts) (optTarget opts)

  graph <-
    ExceptT ((first AppLoad) <$> loadGraphFile (optGraphPath opts))

  liftEither $
    first AppParse $
      validateRunNodes graph (optSource opts) (optTarget opts)

  spec <-
    liftEither $
      first AppAlgorithm $
        resolveAlgorithm graph (optAlgorithm opts)

  let cfg =
        mkRunConfig
          graph
          (optSource opts)
          (optTarget opts)
          (optAlgorithm opts)
          (optThreads opts)

  pregelRun <-
    lift $
      if optSequential opts
        then pure (runSequential cfg spec)
        else runPregel cfg spec

  pure $
    configBanner opts
      ++ [ "Graph loaded:",
           "",
           describeGraph graph,
           "",
           executionHeader opts,
           "",
           describeRun (optVerbose opts) pregelRun
         ]

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
    "  Mode:       "
      ++ if optSequential opts
        then "sequential"
        else "concurrent (async + STM)",
    "  Verbose:    "
      ++ if optVerbose opts then "yes" else "no",
    ""
  ]

executionHeader :: Options -> String
executionHeader opts =
  if optSequential opts
    then "Pregel execution (sequential):"
    else "Pregel execution (async + STM):"
