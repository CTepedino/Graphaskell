module Cli.Error
  ( CliError (..),
    displayCliError,
  )
where

data CliError
  = InvalidNodeId String
  | UnknownAlgorithm String
  | ThreadsTooLow
  | ThreadsExceedCapabilities Int Int
  deriving (Eq, Show)

displayCliError :: CliError -> String
displayCliError err =
  case err of
    InvalidNodeId message ->
      message
    UnknownAlgorithm message ->
      message
    ThreadsTooLow ->
      "Error: --threads must be at least 1"
    ThreadsExceedCapabilities threads maxThreads ->
      unwords
        [ "Error: --threads cannot exceed RTS capabilities",
          "(" ++ show maxThreads ++ ").",
          "Use +RTS -N" ++ show threads ++ " -RTS to increase them."
        ]
