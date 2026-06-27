module Main where

import Algorithm.Spec (resolveAlgorithm, validatePathTarget)
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
import Graph.Types (Graph)
import Output.Trace (describeRun)
import Pregel.Engine (mkRunConfig, runPregel)
import Pregel.Types (PregelRun (..))
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
  spec <-
    except (first AppAlgorithm (resolveAlgorithm graph (optAlgorithm opts)))
  let cfg =
        mkRunConfig
          graph
          (optSource opts)
          (optTarget opts)
          (optThreads opts)
          spec
  pregelRun <- exceptTWith AppPregel =<< liftIO (runPregel cfg spec)
  pure (buildOutput opts graph pregelRun)

except :: Either AppError a -> ExceptT AppError IO a
except = ExceptT . pure

exceptTWith :: (e -> AppError) -> Either e a -> ExceptT AppError IO a
exceptTWith f = ExceptT . pure . first f

buildOutput :: Options -> Graph -> PregelRun -> [String]
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
      ++ " capabilities",
    "  Verbose:    "
      ++ if optVerbose opts then "yes" else "no",
    ""
  ]
