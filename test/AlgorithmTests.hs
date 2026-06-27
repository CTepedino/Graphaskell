module AlgorithmTests (algorithmTests) where

import Algorithm.Error (AlgorithmError (..))
import Algorithm.Spec (resolveAlgorithm, validatePathTarget)
import Fixtures
  ( disconnectedGraphText,
    pageRankGraphText,
    parseFixture,
    resolveFixture,
    runFixture,
    simpleGraphText,
    weightedGraphText,
  )
import Graph.Types (Algorithm (..))
import Pregel.Engine (PregelRun (..), mkRunConfig, runPregel, runSequential)
import Pregel.Types (InputError (..), Result (..))
import Test.HUnit

algorithmTests :: Test
algorithmTests =
  TestList
    [ "BFS finds minimum-hop path" ~: do
        let run = runFixture BFS 0 (Just 4) simpleGraphText
        prResult run @?= PathFound [0, 2, 3, 4] 3,
      "Bellman-Ford finds minimum weighted path" ~: do
        let run = runFixture BellmanFord 0 (Just 3) weightedGraphText
        prResult run @?= PathFound [0, 2, 1, 3] 4,
      "BFS reports no path" ~: do
        let run = runFixture BFS 0 (Just 3) disconnectedGraphText
        prResult run @?= NoPath,
      "BFS without target returns InputError" ~: do
        let run = runFixture BFS 0 Nothing simpleGraphText
        prResult run @?= InputError MissingTarget,
      "validatePathTarget rejects BFS without target" ~: do
        case validatePathTarget BFS Nothing of
          Left MissingPathTarget -> return ()
          other -> assertFailure (show other),
      "validatePathTarget accepts PageRank without target" ~: do
        validatePathTarget PageRank Nothing @?= Right (),
      "Bellman-Ford rejects unweighted graph" ~: do
        let graph = parseFixture simpleGraphText
        case resolveAlgorithm graph BellmanFord of
          Left WeightedGraphRequired -> return ()
          _ -> assertFailure "expected WeightedGraphRequired",
      "connected components from source" ~: do
        let run = runFixture ConnectedComponents 0 Nothing simpleGraphText
        prResult run @?= Components [(0, [0, 1, 2, 3, 4])],
      "connected components in disconnected graph" ~: do
        let run = runFixture ConnectedComponents 0 Nothing disconnectedGraphText
        prResult run @?= Components [(0, [0, 1]), (2, [2, 3])],
      "PageRank produces per-node rankings" ~: do
        let run = runFixture PageRank 0 Nothing pageRankGraphText
        case prResult run of
          Rankings pairs -> length pairs @?= 4
          other -> assertFailure (show other),
      "label propagation produces labels" ~: do
        let run = runFixture LabelPropagation 0 Nothing simpleGraphText
        case prResult run of
          NodeLabels pairs -> length pairs @?= 5
          other -> assertFailure (show other),
      "sequential and concurrent engines agree (BFS)" ~: do
        let graph = parseFixture simpleGraphText
            spec = resolveFixture BFS graph
            cfg = mkRunConfig graph 0 (Just 4) 4
            sequential = runSequential cfg spec
        concurrent <- runPregel cfg spec
        prResult sequential @?= prResult concurrent,
      "threads=1 concurrent matches sequential (Bellman-Ford)" ~: do
        let graph = parseFixture weightedGraphText
            spec = resolveFixture BellmanFord graph
            cfg = mkRunConfig graph 0 (Just 3) 1
            sequential = runSequential cfg spec
        concurrent <- runPregel cfg spec
        prResult sequential @?= prResult concurrent
    ]
