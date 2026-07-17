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

requiresPath :: Algorithm -> Bool
requiresPath BFS = True
requiresPath BellmanFord = True
requiresPath _ = False

requireNode :: AlgorithmError -> Bool -> Maybe NodeId -> Either AlgorithmError ()
requireNode err True Nothing = Left err
requireNode _ _ _ = Right ()

validatePathSource :: Algorithm -> Maybe NodeId -> Either AlgorithmError ()
validatePathSource algo = requireNode MissingPathSource (requiresPath algo)

validatePathTarget :: Algorithm -> Maybe NodeId -> Either AlgorithmError ()
validatePathTarget algo = requireNode MissingPathTarget (requiresPath algo)

resolveAlgorithm :: Algorithm -> SomeAlgorithmSpec
resolveAlgorithm algo = case algo of
  BFS -> SomeAlgorithmSpec bfsSpec
  BellmanFord -> SomeAlgorithmSpec bellmanFordSpec
  PageRank -> SomeAlgorithmSpec pageRankSpec
  ConnectedComponents -> SomeAlgorithmSpec connectedComponentsSpec
  LabelPropagation -> SomeAlgorithmSpec labelPropagationSpec
