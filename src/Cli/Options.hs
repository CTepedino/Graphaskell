module Cli.Options
  ( Options (..),
    parseOptions,
  )
where

import Cli.Error (CliError (..), displayCliError)
import Control.Concurrent (getNumCapabilities)
import Data.Bifunctor (first)
import Data.Char (toUpper)
import Algorithm.Name (Algorithm (..))
import Util.Reading (readNonNegativeInt)
import Graph.Types (NodeId (..))
import Options.Applicative
import System.Exit (die)

data Options = Options
  { optThreads :: Int,
    optGraphPath :: FilePath,
    optSource :: Maybe NodeId,
    optTarget :: Maybe NodeId,
    optAlgorithm :: Algorithm,
    optMaxCapabilities :: Int,
    optVerbose :: Bool
  }
  deriving (Eq, Show)

parseNodeIdOpt :: String -> Either CliError NodeId
parseNodeIdOpt raw =
  first (InvalidNodeId . ("--source/--target: " ++)) (fmap NodeId . readNonNegativeInt $ raw)

parseAlgorithmOpt :: String -> Either CliError Algorithm
parseAlgorithmOpt raw =
  case map toUpper raw of
    "BFS" -> Right BFS
    "BELLMANFORD" -> Right BellmanFord
    "BELLMAN-FORD" -> Right BellmanFord
    "SSSP" -> Right BellmanFord
    "PAGERANK" -> Right PageRank
    "CONNECTEDCOMPONENTS" -> Right ConnectedComponents
    "CC" -> Right ConnectedComponents
    "LABELPROPAGATION" -> Right LabelPropagation
    "LP" -> Right LabelPropagation
    _ ->
      Left
        ( UnknownAlgorithm
            ( "unknown algorithm: "
                ++ raw
                ++ " (use BFS, BELLMANFORD, PAGERANK, CC, LP)"
            )
        )

validateThreads :: Int -> Int -> Either CliError Int
validateThreads maxThreads threads
  | threads < 1 =
      Left ThreadsTooLow
  | threads > maxThreads =
      Left (ThreadsExceedCapabilities threads maxThreads)
  | otherwise =
      Right threads

cliReader :: (String -> Either CliError a) -> ReadM a
cliReader parser =
  eitherReader (first displayCliError . parser)

rawParser ::
  Parser
    ( FilePath,
      Maybe NodeId,
      Maybe NodeId,
      Algorithm,
      Maybe Int,
      Bool
    )
rawParser =
  (,,,,,)
    <$> strOption
      ( long "graph"
          <> short 'g'
          <> metavar "GRAPH"
          <> help "Path to the graph file (graph definition only)"
      )
    <*> optional
      ( option
          (cliReader parseNodeIdOpt)
          ( long "source"
              <> short 's'
              <> metavar "NODE"
              <> help "Source vertex (required for BFS and Bellman-Ford)"
          )
      )
    <*> optional
      ( option
          (cliReader parseNodeIdOpt)
          ( long "target"
              <> short 't'
              <> metavar "NODE"
              <> help "Target vertex (required for path algorithms)"
          )
      )
    <*> option
      (cliReader parseAlgorithmOpt)
      ( long "algorithm"
          <> short 'a'
          <> metavar "ALG"
          <> help "Algorithm: BFS, BELLMANFORD, PAGERANK, CC, LP"
      )
    <*> optional
      ( option
          auto
          ( long "threads"
              <> metavar "N"
              <> help
                "Number of concurrent threads \
                \ (default: all RTS capabilities, see +RTS -N)"
          )
      )
    <*> switch
      ( long "verbose"
          <> short 'v'
          <> help "Detailed per-superstep traces (messages and updates)"
      )

parseOptions :: IO Options
parseOptions = do
  maxThreads <- getNumCapabilities
  (graphPath, source, target, algorithm, mThreads, verbose) <-
    execParser
      ( info
          (helper <*> rawParser)
          ( fullDesc
              <> progDesc
                "Graph path explorer (Pregel BSP model). \
                \ The graph file defines topology; source, target, and algorithm \
                \ are specified via CLI flags."
          )
      )
  threads <-
    case validateThreads maxThreads (maybe maxThreads id mThreads) of
      Left err -> die (displayCliError err)
      Right threadCount -> pure threadCount
  pure
    Options
      { optThreads = threads,
        optGraphPath = graphPath,
        optSource = source,
        optTarget = target,
        optAlgorithm = algorithm,
        optMaxCapabilities = maxThreads,
        optVerbose = verbose
      }
