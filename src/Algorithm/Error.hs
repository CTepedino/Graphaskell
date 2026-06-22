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
      "se requiere --target para calcular un camino (BFS y Bellman-Ford)"
    WeightedGraphRequired ->
      "Bellman-Ford requiere aristas con peso (directiva WEIGHTED en el archivo)"
