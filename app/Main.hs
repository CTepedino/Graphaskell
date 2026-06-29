module Main where

import Algorithm.Spec
  ( SomeAlgorithmSpec (..),
    resolveAlgorithm,
    validatePathSource,
    validatePathTarget,
  )
import Algorithm.Log (DescribeLogEntry (..))
import Algorithm.Types (AlgorithmSpec (..))
import AppError (AppError (..), displayAppError)
import Cli.Options (Options (..), parseOptions)
import Control.Monad.Except (ExceptT (..), runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (first)
import Graph.Parser
  ( describeGraph,
    loadGraphFile,
    validateRunNodesForAlgorithm,
  )
import Graph.Types (ValidGraph, nodeCount)
import Output.Trace (describeRun)
import Pregel.Engine (runPregel)
import Pregel.Types (PregelRun (..), mkRunConfig)
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
execute opts = runExceptT $ do
  except (first AppAlgorithm (validatePathTarget (optAlgorithm opts) (optTarget opts)))
  except (first AppAlgorithm (validatePathSource (optAlgorithm opts) (optSource opts)))
  graph <-
    exceptTWith AppLoad =<< liftIO (loadGraphFile (optGraphPath opts))
  except
    ( first AppParse
        ( validateRunNodesForAlgorithm
            graph
            (optAlgorithm opts)
            (optSource opts)
            (optTarget opts)
        )
    )
  case resolveAlgorithm graph (optAlgorithm opts) of
    Left err ->
      except (Left (AppAlgorithm err))
    Right (SomeAlgorithmSpec spec) -> do
      let cfg =
            mkRunConfig
              graph
              (optSource opts)
              (optTarget opts)
              (optThreads opts)
              (specMaxSupersteps spec (nodeCount graph))
              (optVerbose opts)
      pregelRun <- exceptTWith AppPregel =<< liftIO (runPregel cfg spec)
      pure (buildOutput opts graph pregelRun)

except :: Either AppError a -> ExceptT AppError IO a
except = ExceptT . pure

exceptTWith :: (e -> AppError) -> Either e a -> ExceptT AppError IO a
exceptTWith f = ExceptT . pure . first f

buildOutput :: DescribeLogEntry log => Options -> ValidGraph -> PregelRun log -> [String]
buildOutput opts graph pregelRun =
  configBanner opts
    ++ [ "Graph loaded:",
         "",
         describeGraph graph,
         "",
         "Pregel execution (parallel vertex compute + STM):",
         "",
         describeRun (optVerbose opts) pregelRun
       ]

configBanner :: Options -> [String]
configBanner opts =
  [ "Graphaskell",
    "",
    "  Graph:      " ++ optGraphPath opts,
    "  Source:     " ++ sourceBanner opts,
    "  Target:     " ++ maybe "—" show (optTarget opts),
    "  Algorithm:  " ++ show (optAlgorithm opts),
    "  Threads:    "
      ++ show (optThreads opts)
      ++ " / "
      ++ show (optMaxCapabilities opts)
    ]

sourceBanner :: Options -> String
sourceBanner opts =
  case optSource opts of
    Just nodeId ->
      show nodeId
    Nothing ->
      "—"
