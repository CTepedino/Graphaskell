module Algorithm.Spec
  ( AlgorithmSpec,
    resolveAlgorithm,
    validatePathTarget,
  )
where

import Algorithm.BFS (bfsSpec)
import Algorithm.BellmanFord (bellmanFordSpec)
import Algorithm.Common (validateWeightedGraph)
import Algorithm.ConnectedComponents (connectedComponentsSpec)
import Algorithm.Error (AlgorithmError (..))
import Algorithm.LabelPropagation (labelPropagationSpec)
import Algorithm.PageRank (pageRankSpec)
import Algorithm.Types
import Graph.Types

validatePathTarget :: Algorithm -> Maybe NodeId -> Either AlgorithmError ()
validatePathTarget BFS Nothing = Left MissingPathTarget
validatePathTarget BellmanFord Nothing = Left MissingPathTarget
validatePathTarget _ _ = Right ()

resolveAlgorithm :: Graph -> Algorithm -> Either AlgorithmError AlgorithmSpec
resolveAlgorithm _ BFS = Right bfsSpec
resolveAlgorithm graph BellmanFord = do
  validateWeightedGraph graph
  pure bellmanFordSpec
resolveAlgorithm _ PageRank = Right pageRankSpec
resolveAlgorithm _ ConnectedComponents = Right connectedComponentsSpec
resolveAlgorithm _ LabelPropagation = Right labelPropagationSpec
