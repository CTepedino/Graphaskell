module Algorithm.Spec
  ( AlgorithmSpec,
    GlobalAlgorithmSpec,
    PathAlgorithmSpec,
    SomeAlgorithmSpec (..),
    globalRunSpec,
    pathRunSpec,
    resolveAlgorithm,
    validatePathTarget,
  )
where

import Algorithm.BFS (bfsPathSpec)
import Algorithm.BellmanFord (bellmanFordPathSpec)
import Algorithm.Common (validateWeightedGraph)
import Algorithm.ConnectedComponents (connectedComponentsGlobalSpec)
import Algorithm.Error (AlgorithmError (..))
import Algorithm.LabelPropagation (labelPropagationGlobalSpec)
import Algorithm.PageRank (pageRankGlobalSpec)
import Algorithm.Types
import Graph.Types

validatePathTarget :: Algorithm -> Maybe NodeId -> Either AlgorithmError ()
validatePathTarget BFS Nothing = Left MissingPathTarget
validatePathTarget BellmanFord Nothing = Left MissingPathTarget
validatePathTarget _ _ = Right ()

resolveAlgorithm :: Graph -> Algorithm -> Either AlgorithmError SomeAlgorithmSpec
resolveAlgorithm _ BFS = Right (SomePathAlgorithmSpec bfsPathSpec)
resolveAlgorithm graph BellmanFord = do
  validateWeightedGraph graph
  pure (SomePathAlgorithmSpec bellmanFordPathSpec)
resolveAlgorithm _ PageRank = Right (SomeGlobalAlgorithmSpec pageRankGlobalSpec)
resolveAlgorithm _ ConnectedComponents =
  Right (SomeGlobalAlgorithmSpec connectedComponentsGlobalSpec)
resolveAlgorithm _ LabelPropagation =
  Right (SomeGlobalAlgorithmSpec labelPropagationGlobalSpec)
