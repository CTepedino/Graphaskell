module Algorithm.Spec
  ( SomeAlgorithmSpec (..),
    requirePathTarget,
    resolveAlgorithm,
    validatePathSource,
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
import Algorithm.Name (Algorithm (..))
import Graph.Types (ValidGraph, NodeId (..))

requirePathTarget :: Maybe NodeId -> Either AlgorithmError NodeId
requirePathTarget = maybe (Left MissingPathTarget) Right

pathAlgorithm :: Algorithm -> Bool
pathAlgorithm BFS = True
pathAlgorithm BellmanFord = True
pathAlgorithm _ = False

validatePathSource :: Algorithm -> Maybe NodeId -> Either AlgorithmError ()
validatePathSource algo source
  | pathAlgorithm algo, Nothing <- source =
      Left MissingPathSource
  | otherwise =
      Right ()

validatePathTarget :: Algorithm -> Maybe NodeId -> Either AlgorithmError ()
validatePathTarget algo target
  | pathAlgorithm algo =
      requirePathTarget target >> Right ()
  | otherwise =
      Right ()

resolveAlgorithm :: ValidGraph -> Algorithm -> Either AlgorithmError SomeAlgorithmSpec
resolveAlgorithm _ BFS = Right (SomeAlgorithmSpec bfsSpec)
resolveAlgorithm graph BellmanFord = do
  validateWeightedGraph graph
  pure (SomeAlgorithmSpec bellmanFordSpec)
resolveAlgorithm _ PageRank = Right (SomeAlgorithmSpec pageRankSpec)
resolveAlgorithm _ ConnectedComponents =
  Right (SomeAlgorithmSpec connectedComponentsSpec)
resolveAlgorithm _ LabelPropagation =
  Right (SomeAlgorithmSpec labelPropagationSpec)
