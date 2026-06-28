module Algorithm.Spec
  ( SomeAlgorithmSpec (..),
    requirePathTarget,
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
import Algorithm.Types (SomeAlgorithmSpec (..))
import Graph.Types

requirePathTarget :: Maybe NodeId -> Either AlgorithmError NodeId
requirePathTarget = maybe (Left MissingPathTarget) Right

validatePathTarget :: Algorithm -> Maybe NodeId -> Either AlgorithmError ()
validatePathTarget BFS target = requirePathTarget target >> Right ()
validatePathTarget BellmanFord target = requirePathTarget target >> Right ()
validatePathTarget _ _ = Right ()

resolveAlgorithm :: Graph -> Algorithm -> Either AlgorithmError SomeAlgorithmSpec
resolveAlgorithm _ BFS = Right (SomeAlgorithmSpec bfsSpec)
resolveAlgorithm graph BellmanFord = do
  validateWeightedGraph graph
  pure (SomeAlgorithmSpec bellmanFordSpec)
resolveAlgorithm _ PageRank = Right (SomeAlgorithmSpec pageRankSpec)
resolveAlgorithm _ ConnectedComponents =
  Right (SomeAlgorithmSpec connectedComponentsSpec)
resolveAlgorithm _ LabelPropagation =
  Right (SomeAlgorithmSpec labelPropagationSpec)
