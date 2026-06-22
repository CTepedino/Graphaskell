module Cli.Options
  ( Options (..),
    parseOptions,
  )
where

import Control.Concurrent (getNumCapabilities)
import Control.Monad (when)
import Options.Applicative
import System.Exit (die)

data Options = Options
  { optThreads :: Int,
    optGraphPath :: FilePath,
    optMaxCapabilities :: Int,
    optVerbose :: Bool
  }
  deriving (Eq, Show)

rawParser :: Parser (Maybe Int, Bool, FilePath)
rawParser =
  (,,)
    <$> optional
      ( option
          auto
          ( long "threads"
              <> short 't'
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
    <*> strArgument
      ( metavar "GRAFO"
          <> help "Path al archivo de grafo"
      )

parseOptions :: IO Options
parseOptions = do
  maxThreads <- getNumCapabilities
  (mThreads, verbose, graphPath) <-
    execParser
      ( info
          (helper <*> rawParser)
          ( fullDesc
              <> progDesc "Explorador de caminos en grafos (modelo Pregel)"
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
        optMaxCapabilities = maxThreads,
        optVerbose = verbose
      }
