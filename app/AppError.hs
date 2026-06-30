module AppError
  ( AppError (..),
    displayAppError,
  )
where

import Algorithm.Error (AlgorithmError, displayAlgorithmError)
import Cli.Error (CliError, displayCliError)
import Graph.Load (LoadGraphError, displayLoadGraphError)
import Graph.ValidationError (GraphValidationError, displayGraphValidationError)
import Pregel.Error (PregelError, displayPregelError)

data AppError
  = AppCli CliError
  | AppAlgorithm AlgorithmError
  | AppGraphValidation GraphValidationError
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
    AppGraphValidation validationError ->
      "Error in source/target: " ++ displayGraphValidationError validationError
    AppLoad loadError ->
      "Error loading graph: " ++ displayLoadGraphError loadError
    AppPregel pregelError ->
      displayPregelError pregelError
