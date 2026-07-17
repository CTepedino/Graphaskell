module Algorithm.Spec
  ( SomeAlgorithmSpec (..),
    resolveAlgorithm,
    validatePathSource,
    validatePathTarget,
  )
where

import Algorithm.BFS (bfsSpec)
import Algorithm.BellmanFord (bellmanFordSpec)
import Algorithm.ConnectedComponents (connectedComponentsSpec)
import Algorithm.Error (AlgorithmError (..))
import Algorithm.LabelPropagation (labelPropagationSpec)
import Algorithm.PageRank (pageRankSpec)
import Algorithm.Types (SomeAlgorithmSpec (..))
import Algorithm.Name (Algorithm (..))
import Graph.Types (NodeId)

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

resolveAlgorithm :: Algorithm -> SomeAlgorithmSpec
resolveAlgorithm algo = case algo of
  BFS -> SomeAlgorithmSpec bfsSpec
  BellmanFord -> SomeAlgorithmSpec bellmanFordSpec
  PageRank -> SomeAlgorithmSpec pageRankSpec
  ConnectedComponents -> SomeAlgorithmSpec connectedComponentsSpec
  LabelPropagation -> SomeAlgorithmSpec labelPropagationSpec
