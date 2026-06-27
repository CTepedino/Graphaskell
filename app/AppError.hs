module AppError
  ( AppError (..),
    displayAppError,
  )
where

import Algorithm.Error (AlgorithmError, displayAlgorithmError)
import Graph.ParseError (ParseError, displayParseError)
import Graph.Parser (LoadGraphError, displayLoadGraphError)
import Pregel.Error (PregelError, displayPregelError)

data AppError
  = AppAlgorithm AlgorithmError
  | AppParse ParseError
  | AppLoad LoadGraphError
  | AppPregel PregelError
  deriving (Eq, Show)

displayAppError :: AppError -> String
displayAppError err =
  case err of
    AppAlgorithm algorithmError ->
      displayAlgorithmError algorithmError
    AppParse parseError ->
      "Error in source/target: " ++ displayParseError parseError
    AppLoad loadError ->
      "Error loading graph: " ++ displayLoadGraphError loadError
    AppPregel pregelError ->
      displayPregelError pregelError
