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
    _ -> Left ("debe ser un entero >= 0: " ++ raw)

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
        ( "algoritmo desconocido: "
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
          <> metavar "GRAFO"
          <> help "Path al archivo de grafo (solo definicion del grafo)"
      )
    <*> option
      (eitherReader parseNodeIdOpt)
      ( long "source"
          <> short 's'
          <> metavar "NODO"
          <> help "Vertice origen de la busqueda"
      )
    <*> optional
      ( option
          (eitherReader parseNodeIdOpt)
          ( long "target"
              <> short 't'
              <> metavar "NODO"
              <> help "Vertice destino (requerido para calcular un camino)"
          )
      )
    <*> option
      (eitherReader parseAlgorithmOpt)
      ( long "algorithm"
          <> short 'a'
          <> metavar "ALG"
          <> help "Algoritmo: BFS, BELLMANFORD, PAGERANK, CC, LP"
      )
    <*> optional
      ( option
          auto
          ( long "threads"
              <> metavar "N"
              <> help
                "Cantidad de threads concurrentes \
                \ (default: todas las capacidades del runtime, ver +RTS -N)"
          )
      )
    <*> switch
      ( long "verbose"
          <> short 'v'
          <> help "Trazas detalladas por superstep (mensajes y actualizaciones)"
      )
    <*> switch
      ( long "sequential"
          <> help "Ejecutar el motor Pregel en modo secuencial (sin async/STM)"
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
                "Explorador de caminos en grafos (modelo Pregel). \
                \ El archivo define el grafo; origen, destino y algoritmo \
                \ se indican por linea de comandos."
          )
      )
  let threads = maybe maxThreads id mThreads
  when (threads < 1) $
    die "Error: --threads debe ser al menos 1"
  when (threads > maxThreads) $
    die $
      unwords
        [ "Error: --threads no puede superar las capacidades del runtime",
          "(" ++ show maxThreads ++ ").",
          "Usá +RTS -N" ++ show threads ++ " -RTS para aumentarlas."
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
