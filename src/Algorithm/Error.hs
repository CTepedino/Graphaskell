module Algorithm.Error
  ( AlgorithmError (..),
    displayAlgorithmError,
  )
where

import Graph.Types (NodeId)

data AlgorithmError
  = MissingPathSource
  | MissingPathTarget
  | TargetNodeMissing NodeId
  | WeightedGraphRequired
  deriving (Eq, Show)

displayAlgorithmError :: AlgorithmError -> String
displayAlgorithmError err =
  case err of
    MissingPathSource ->
      "--source is required for path algorithms (BFS and Bellman-Ford)"
    MissingPathTarget ->
      "--target is required to compute a path (BFS and Bellman-Ford)"
    TargetNodeMissing nodeId ->
      "node " ++ show nodeId ++ " does not exist"
    WeightedGraphRequired ->
      "Bellman-Ford requires weighted edges (WEIGHTED directive in the graph file)"
