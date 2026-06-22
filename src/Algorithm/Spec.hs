module Algorithm.Spec
  ( AlgorithmSpec,
    resolveAlgorithm,
  )
where

import Algorithm.BFS (bfsSpec)
import Algorithm.DFS (dfsSpec)
import Algorithm.Dijkstra (dijkstraSpec, validateWeightedGraph)
import Algorithm.Error (AlgorithmError (..))
import Algorithm.Types
import Graph.Types

resolveAlgorithm :: Graph -> Algorithm -> Either AlgorithmError AlgorithmSpec
resolveAlgorithm _ BFS = Right bfsSpec
resolveAlgorithm _ DFS = Right dfsSpec
resolveAlgorithm graph Dijkstra = do
  validateWeightedGraph graph
  pure dijkstraSpec
