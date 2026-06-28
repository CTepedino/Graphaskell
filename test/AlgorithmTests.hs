module AlgorithmTests (algorithmTests) where

import Algorithm.Error (AlgorithmError (..))
import Algorithm.Result (Result (..))
import Algorithm.Spec (resolveAlgorithm, validatePathSource, validatePathTarget)
import Fixtures
  ( FixtureError (..),
    disconnectedGraphText,
    pageRankGraphText,
    parseFixture,
    resolveFixture,
    runFixtureEither,
    simpleGraphText,
    weightedGraphText,
  )
import Graph.Types (Algorithm (..), Distance (..), NodeId (..))
import Output.Run (somePregelResult)
import Pregel.Error (PregelError (..))
import Test.HUnit
import TestSupport
  ( assertEnginesAgreeSome,
    assertRankingsApprox,
    assertValidBfsPath,
    labelPropagationExpected,
    pageRankExpected,
    requireFixture,
  )

algorithmTests :: Test
algorithmTests =
  TestList
    [ "BFS finds a minimum-hop path" ~: do
        graph <- requireFixture (parseFixture simpleGraphText)
        someRun <-
          requireFixture (runFixtureEither BFS (NodeId 0) (Just (NodeId 4)) simpleGraphText)
        assertValidBfsPath graph (NodeId 0) (NodeId 4) (Distance 3) (somePregelResult someRun),
      "Bellman-Ford finds minimum weighted path" ~: do
        someRun <-
          requireFixture (runFixtureEither BellmanFord (NodeId 0) (Just (NodeId 3)) weightedGraphText)
        somePregelResult someRun @?= PathFound [NodeId 0, NodeId 2, NodeId 1, NodeId 3] (Distance 4),
      "BFS reports no path" ~: do
        someRun <-
          requireFixture (runFixtureEither BFS (NodeId 0) (Just (NodeId 3)) disconnectedGraphText)
        somePregelResult someRun @?= NoPath,
      "BFS without target fails at extraction" ~: do
        let result = runFixtureEither BFS (NodeId 0) Nothing simpleGraphText
        case result of
          Left (FixtureRun (ResultExtraction MissingPathTarget)) ->
            pure ()
          Left err ->
            assertFailure ("expected MissingPathTarget extraction, got " ++ show err)
          Right _ ->
            assertFailure "expected extraction failure, got successful run",
      "validatePathTarget rejects BFS without target" ~: do
        validatePathTarget BFS Nothing @?= Left MissingPathTarget,
      "validatePathTarget rejects Bellman-Ford without target" ~: do
        validatePathTarget BellmanFord Nothing @?= Left MissingPathTarget,
      "validatePathSource rejects BFS without source" ~: do
        validatePathSource BFS Nothing @?= Left MissingPathSource,
      "validatePathSource rejects Bellman-Ford without source" ~: do
        validatePathSource BellmanFord Nothing @?= Left MissingPathSource,
      "validatePathSource accepts PageRank without source" ~: do
        validatePathSource PageRank Nothing @?= Right (),
      "validatePathTarget accepts PageRank without target" ~: do
        validatePathTarget PageRank Nothing @?= Right (),
      "Bellman-Ford rejects unweighted graph" ~: do
        graph <- requireFixture (parseFixture simpleGraphText)
        case resolveAlgorithm graph BellmanFord of
          Left WeightedGraphRequired -> pure ()
          _ -> assertFailure "expected WeightedGraphRequired",
      "connected components list all groups in connected graph" ~: do
        someRun <-
          requireFixture (runFixtureEither ConnectedComponents (NodeId 0) Nothing simpleGraphText)
        somePregelResult someRun @?= Components [(NodeId 0, [NodeId 0, NodeId 1, NodeId 2, NodeId 3, NodeId 4])],
      "connected components list all groups in disconnected graph" ~: do
        someRun <-
          requireFixture (runFixtureEither ConnectedComponents (NodeId 0) Nothing disconnectedGraphText)
        somePregelResult someRun @?= Components [(NodeId 0, [NodeId 0, NodeId 1]), (NodeId 2, [NodeId 2, NodeId 3])],
      "PageRank converges to expected rankings" ~: do
        someRun <-
          requireFixture (runFixtureEither PageRank (NodeId 0) Nothing pageRankGraphText)
        assertRankingsApprox 1e-6 pageRankExpected (somePregelResult someRun),
      "label propagation converges to expected labels" ~: do
        someRun <-
          requireFixture (runFixtureEither LabelPropagation (NodeId 0) Nothing simpleGraphText)
        somePregelResult someRun @?= NodeLabels labelPropagationExpected,
      "sequential and concurrent engines agree on BFS" ~: do
        graph <- requireFixture (parseFixture simpleGraphText)
        someSpec <- requireFixture (resolveFixture BFS graph)
        assertEnginesAgreeSome graph (Just (NodeId 0)) (Just (NodeId 4)) 4 someSpec,
      "sequential and concurrent engines agree on Bellman-Ford" ~: do
        graph <- requireFixture (parseFixture weightedGraphText)
        someSpec <- requireFixture (resolveFixture BellmanFord graph)
        assertEnginesAgreeSome graph (Just (NodeId 0)) (Just (NodeId 3)) 1 someSpec,
      "sequential and concurrent engines agree on connected components" ~: do
        graph <- requireFixture (parseFixture disconnectedGraphText)
        someSpec <- requireFixture (resolveFixture ConnectedComponents graph)
        assertEnginesAgreeSome graph Nothing Nothing 2 someSpec,
      "sequential and concurrent engines agree on PageRank" ~: do
        graph <- requireFixture (parseFixture pageRankGraphText)
        someSpec <- requireFixture (resolveFixture PageRank graph)
        assertEnginesAgreeSome graph Nothing Nothing 2 someSpec,
      "sequential and concurrent engines agree on label propagation" ~: do
        graph <- requireFixture (parseFixture simpleGraphText)
        someSpec <- requireFixture (resolveFixture LabelPropagation graph)
        assertEnginesAgreeSome graph Nothing Nothing 2 someSpec
    ]
