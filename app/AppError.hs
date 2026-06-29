module AppError
  ( AppError (..),
    displayAppError,
  )
where

import Algorithm.Error (AlgorithmError, displayAlgorithmError)
import Cli.Error (CliError, displayCliError)
import Graph.ParseError (ParseError, displayParseError)
import Graph.Parser (LoadGraphError, displayLoadGraphError)
import Pregel.Error (PregelError, displayPregelError)

data AppError
  = AppCli CliError
  | AppAlgorithm AlgorithmError
  | AppParse ParseError
  | AppLoad LoadGraphError
  | AppPregel PregelError
  deriving (Eq, Show)

displayAppError :: AppError -> String
displayAppError err =
  case err of
    AppCli cliError ->
      displayCliError cliError
    AppAlgorithm algorithmError ->
      displayAlgorithmError algorithmError
    AppParse parseError ->
      "Error in source/target: " ++ displayParseError parseError
    AppLoad loadError ->
      "Error loading graph: " ++ displayLoadGraphError loadError
    AppPregel pregelError ->
      displayPregelError pregelError
