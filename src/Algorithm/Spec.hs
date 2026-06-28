module Algorithm.Spec
  ( SomeAlgorithmSpec (..),
    requirePathTarget,
    resolveAlgorithm,
    resolveRunSource,
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
import Graph.Types (ValidGraph, Algorithm (..), NodeId (..))

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

resolveRunSource :: Algorithm -> Maybe NodeId -> NodeId
resolveRunSource algo source =
  case source of
    Just nodeId ->
      nodeId
    Nothing ->
      NodeId 0

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
