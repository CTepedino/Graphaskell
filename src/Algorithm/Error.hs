module Algorithm.Error
  ( AlgorithmError (..),
    displayAlgorithmError,
  )
where

import Graph.Types (Algorithm (..))

data AlgorithmError
  = NotImplemented Algorithm
  | DijkstraRequiresWeightedGraph
  deriving (Eq, Show)

displayAlgorithmError :: AlgorithmError -> String
displayAlgorithmError err =
  case err of
    NotImplemented algorithm ->
      "Algoritmo "
        ++ show algorithm
        ++ " aun no esta implementado"
    DijkstraRequiresWeightedGraph ->
      "Dijkstra requiere aristas con peso positivo (directiva WEIGHTED en el archivo)"
