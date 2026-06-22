module Algorithm.Error
  ( AlgorithmError (..),
    displayAlgorithmError,
  )
where

data AlgorithmError
  = MissingPathTarget
  | WeightedGraphRequired
  deriving (Eq, Show)

displayAlgorithmError :: AlgorithmError -> String
displayAlgorithmError err =
  case err of
    MissingPathTarget ->
      "--target is required to compute a path (BFS and Bellman-Ford)"
    WeightedGraphRequired ->
      "Bellman-Ford requires weighted edges (WEIGHTED directive in the graph file)"
