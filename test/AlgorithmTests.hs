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
import Pregel.Engine (mkRunConfig)
import Pregel.Types (InputError (..), PregelRun (..), Result (..))
import Test.HUnit
import TestSupport
  ( assertEnginesAgree,
    assertRankingsApprox,
    assertValidBfsPath,
    labelPropagationExpected,
    pageRankExpected,
  )

algorithmTests :: Test
algorithmTests =
  TestList
    [ "BFS finds a minimum-hop path" ~: do
        let graph = parseFixture simpleGraphText
            run = runFixture BFS 0 (Just 4) simpleGraphText
        assertValidBfsPath graph 0 4 3 (prResult run),
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
        validatePathTarget BFS Nothing @?= Left MissingPathTarget,
      "validatePathTarget rejects Bellman-Ford without target" ~: do
        validatePathTarget BellmanFord Nothing @?= Left MissingPathTarget,
      "validatePathTarget accepts PageRank without target" ~: do
        validatePathTarget PageRank Nothing @?= Right (),
      "Bellman-Ford rejects unweighted graph" ~: do
        let graph = parseFixture simpleGraphText
        case resolveAlgorithm graph BellmanFord of
          Left WeightedGraphRequired -> return ()
          _ -> assertFailure "expected WeightedGraphRequired",
      "connected components list all groups in connected graph" ~: do
        let run = runFixture ConnectedComponents 0 Nothing simpleGraphText
        prResult run @?= Components [(0, [0, 1, 2, 3, 4])],
      "connected components list all groups in disconnected graph" ~: do
        let run = runFixture ConnectedComponents 0 Nothing disconnectedGraphText
        prResult run @?= Components [(0, [0, 1]), (2, [2, 3])],
      "PageRank converges to expected rankings" ~: do
        let run = runFixture PageRank 0 Nothing pageRankGraphText
        assertRankingsApprox 1e-6 pageRankExpected (prResult run),
      "label propagation converges to expected labels" ~: do
        let run = runFixture LabelPropagation 0 Nothing simpleGraphText
        prResult run @?= NodeLabels labelPropagationExpected,
      "sequential and concurrent engines agree on BFS" ~:
        let graph = parseFixture simpleGraphText
            spec = resolveFixture BFS graph
         in assertEnginesAgree (mkRunConfig graph 0 (Just 4) 4 spec) spec,
      "sequential and concurrent engines agree on Bellman-Ford" ~:
        let graph = parseFixture weightedGraphText
            spec = resolveFixture BellmanFord graph
         in assertEnginesAgree (mkRunConfig graph 0 (Just 3) 1 spec) spec,
      "sequential and concurrent engines agree on connected components" ~:
        let graph = parseFixture disconnectedGraphText
            spec = resolveFixture ConnectedComponents graph
         in assertEnginesAgree (mkRunConfig graph 0 Nothing 2 spec) spec,
      "sequential and concurrent engines agree on PageRank" ~:
        let graph = parseFixture pageRankGraphText
            spec = resolveFixture PageRank graph
         in assertEnginesAgree (mkRunConfig graph 0 Nothing 2 spec) spec,
      "sequential and concurrent engines agree on label propagation" ~:
        let graph = parseFixture simpleGraphText
            spec = resolveFixture LabelPropagation graph
         in assertEnginesAgree (mkRunConfig graph 0 Nothing 2 spec) spec
    ]
