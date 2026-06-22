module Cli.Options
  ( Options (..),
    parseOptions,
  )
where

import Control.Concurrent (getNumCapabilities)
import Control.Monad (when)
import Data.Char (toUpper)
import Graph.Types (Algorithm (..), NodeId)
import Options.Applicative
import System.Exit (die)

data Options = Options
  { optThreads :: Int,
    optGraphPath :: FilePath,
    optSource :: NodeId,
    optTarget :: Maybe NodeId,
    optAlgorithm :: Algorithm,
    optMaxCapabilities :: Int,
    optVerbose :: Bool,
    optSequential :: Bool
  }
  deriving (Eq, Show)

parseNodeIdOpt :: String -> Either String NodeId
parseNodeIdOpt raw =
  case reads raw of
    [(n, "")] | n >= 0 -> Right n
    _ -> Left ("must be an integer >= 0: " ++ raw)

parseAlgorithmOpt :: String -> Either String Algorithm
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
        ( "unknown algorithm: "
            ++ raw
            ++ " (use BFS, BELLMANFORD, PAGERANK, CC, LP)"
        )

rawParser ::
  Parser
    ( FilePath,
      NodeId,
      Maybe NodeId,
      Algorithm,
      Maybe Int,
      Bool,
      Bool
    )
rawParser =
  (,,,,,,)
    <$> strOption
      ( long "graph"
          <> short 'g'
          <> metavar "GRAPH"
          <> help "Path to the graph file (graph definition only)"
      )
    <*> option
      (eitherReader parseNodeIdOpt)
      ( long "source"
          <> short 's'
          <> metavar "NODE"
          <> help "Source vertex"
      )
    <*> optional
      ( option
          (eitherReader parseNodeIdOpt)
          ( long "target"
              <> short 't'
              <> metavar "NODE"
              <> help "Target vertex (required for path algorithms)"
          )
      )
    <*> option
      (eitherReader parseAlgorithmOpt)
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
    <*> switch
      ( long "sequential"
          <> help "Run the Pregel engine sequentially (no async/STM)"
      )

parseOptions :: IO Options
parseOptions = do
  maxThreads <- getNumCapabilities
  (graphPath, source, target, algorithm, mThreads, verbose, sequential) <-
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
  let threads = maybe maxThreads id mThreads
  when (threads < 1) $
    die "Error: --threads must be at least 1"
  when (threads > maxThreads) $
    die $
      unwords
        [ "Error: --threads cannot exceed RTS capabilities",
          "(" ++ show maxThreads ++ ").",
          "Use +RTS -N" ++ show threads ++ " -RTS to increase them."
        ]
  pure
    Options
      { optThreads = threads,
        optGraphPath = graphPath,
        optSource = source,
        optTarget = target,
        optAlgorithm = algorithm,
        optMaxCapabilities = maxThreads,
        optVerbose = verbose,
        optSequential = sequential
      }
