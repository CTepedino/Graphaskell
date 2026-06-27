module AlgorithmTests (algorithmTests) where

import Algorithm.Error (AlgorithmError (..))
import Algorithm.Result (Result (..))
import Algorithm.Spec (resolveAlgorithm, validatePathTarget)
import Fixtures
  ( disconnectedGraphText,
    pageRankGraphText,
    parseFixture,
    resolveFixture,
    runFixture,
    runFixtureEither,
    simpleGraphText,
    weightedGraphText,
  )
import Graph.Types (Algorithm (..))
import Pregel.Error (PregelError (..))
import Pregel.Types (somePregelResult)
import Test.HUnit
import TestSupport
  ( assertEnginesAgreeSome,
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
        assertValidBfsPath graph 0 4 3 (somePregelResult run),
      "Bellman-Ford finds minimum weighted path" ~: do
        let run = runFixture BellmanFord 0 (Just 3) weightedGraphText
        somePregelResult run @?= PathFound [0, 2, 1, 3] 4,
      "BFS reports no path" ~: do
        let run = runFixture BFS 0 (Just 3) disconnectedGraphText
        somePregelResult run @?= NoPath,
      "BFS without target fails at extraction" ~:
        ( case runFixtureEither BFS 0 Nothing simpleGraphText of
            Left (ResultExtraction MissingPathTarget) ->
              return ()
            Left err ->
              assertFailure ("expected MissingPathTarget extraction, got " ++ show err)
            Right _ ->
              assertFailure "expected extraction failure, got successful run"
        ),
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
        somePregelResult run @?= Components [(0, [0, 1, 2, 3, 4])],
      "connected components list all groups in disconnected graph" ~: do
        let run = runFixture ConnectedComponents 0 Nothing disconnectedGraphText
        somePregelResult run @?= Components [(0, [0, 1]), (2, [2, 3])],
      "PageRank converges to expected rankings" ~: do
        let run = runFixture PageRank 0 Nothing pageRankGraphText
        assertRankingsApprox 1e-6 pageRankExpected (somePregelResult run),
      "label propagation converges to expected labels" ~: do
        let run = runFixture LabelPropagation 0 Nothing simpleGraphText
        somePregelResult run @?= NodeLabels labelPropagationExpected,
      "sequential and concurrent engines agree on BFS" ~:
        assertEnginesAgreeSome
          (parseFixture simpleGraphText)
          0
          (Just 4)
          4
          (resolveFixture BFS (parseFixture simpleGraphText)),
      "sequential and concurrent engines agree on Bellman-Ford" ~:
        assertEnginesAgreeSome
          (parseFixture weightedGraphText)
          0
          (Just 3)
          1
          (resolveFixture BellmanFord (parseFixture weightedGraphText)),
      "sequential and concurrent engines agree on connected components" ~:
        assertEnginesAgreeSome
          (parseFixture disconnectedGraphText)
          0
          Nothing
          2
          (resolveFixture ConnectedComponents (parseFixture disconnectedGraphText)),
      "sequential and concurrent engines agree on PageRank" ~:
        assertEnginesAgreeSome
          (parseFixture pageRankGraphText)
          0
          Nothing
          2
          (resolveFixture PageRank (parseFixture pageRankGraphText)),
      "sequential and concurrent engines agree on label propagation" ~:
        assertEnginesAgreeSome
          (parseFixture simpleGraphText)
          0
          Nothing
          2
          (resolveFixture LabelPropagation (parseFixture simpleGraphText))
    ]
