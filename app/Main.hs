module Main where

import Algorithm.Error (AlgorithmError (..))
import Algorithm.Log (DescribeLogEntry (..))
import Algorithm.Spec
  ( SomeAlgorithmSpec (..),
    resolveAlgorithm,
    validatePathTarget,
  )
import Algorithm.Types (GlobalAlgorithmSpec (..), PathAlgorithmSpec (..))
import AppError (AppError (..), displayAppError)
import Cli.Options (Options (..), parseOptions)
import Control.Monad.Except (ExceptT (..), runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (first)
import Graph.Parser
  ( describeGraph,
    loadGraphFile,
    validateRunNodes,
  )
import Graph.Types (Graph, nodeCount)
import Output.Trace (describeRun)
import Pregel.Engine (runGlobalPregel, runPathPregel)
import Pregel.Types (PregelRun (..), mkPathRunConfig, mkRunConfig)
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
  graph <-
    exceptTWith AppLoad =<< liftIO (loadGraphFile (optGraphPath opts))
  except (first AppParse (validateRunNodes graph (optSource opts) (optTarget opts)))
  case resolveAlgorithm graph (optAlgorithm opts) of
    Left err ->
      except (Left (AppAlgorithm err))
    Right (SomePathAlgorithmSpec pathSpec) -> do
      target <-
        except
          ( maybe
              (Left (AppAlgorithm MissingPathTarget))
              Right
              (optTarget opts)
          )
      let prc =
            mkPathRunConfig
              graph
              (optSource opts)
              target
              (optThreads opts)
              (psMaxSupersteps pathSpec (nodeCount graph))
      pregelRun <- exceptTWith AppPregel =<< liftIO (runPathPregel prc pathSpec)
      pure (buildOutput opts graph pregelRun)
    Right (SomeGlobalAlgorithmSpec globalSpec) -> do
      let cfg =
            mkRunConfig
              graph
              (optSource opts)
              (optThreads opts)
              (globalMaxSupersteps globalSpec (nodeCount graph))
      pregelRun <- exceptTWith AppPregel =<< liftIO (runGlobalPregel cfg globalSpec)
      pure (buildOutput opts graph pregelRun)

except :: Either AppError a -> ExceptT AppError IO a
except = ExceptT . pure

exceptTWith :: (e -> AppError) -> Either e a -> ExceptT AppError IO a
exceptTWith f = ExceptT . pure . first f

buildOutput :: DescribeLogEntry log => Options -> Graph -> PregelRun log -> [String]
buildOutput opts graph pregelRun =
  configBanner opts
    ++ [ "Graph loaded:",
         "",
         describeGraph graph,
         "",
         "Pregel execution (async + STM):",
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
    ]
