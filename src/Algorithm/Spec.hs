module Algorithm.Spec
  ( SomeAlgorithmSpec (..),
    requirePathTarget,
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
import Algorithm.Types (SomeAlgorithmSpec (..))
import Graph.Types

requirePathTarget :: Maybe NodeId -> Either AlgorithmError NodeId
requirePathTarget = maybe (Left MissingPathTarget) Right

validatePathTarget :: Algorithm -> Maybe NodeId -> Either AlgorithmError ()
validatePathTarget BFS target = requirePathTarget target >> Right ()
validatePathTarget BellmanFord target = requirePathTarget target >> Right ()
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
