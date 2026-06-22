module AlgorithmTests (algorithmTests) where

import Algorithm.Error (AlgorithmError (..))
import Algorithm.Spec (resolveAlgorithm, validatePathTarget)
import Fixtures
  ( disconnectedGraphText,
    mkRunConfigFor,
    pageRankGraphText,
    parseFixture,
    resolveFixture,
    runFixture,
    simpleGraphText,
    weightedGraphText,
  )
import Graph.Types (Algorithm (..))
import Pregel.Engine (PregelRun (..), runPregel, runSequential)
import Pregel.Types (InputError (..), Result (..))
import Test.HUnit

algorithmTests :: Test
algorithmTests =
  TestList
    [ "BFS encuentra camino minimo en saltos" ~: do
        let run = runFixture BFS 0 (Just 4) simpleGraphText
        prResult run @?= PathFound [0, 2, 3, 4] 3,
      "Bellman-Ford encuentra camino minimo ponderado" ~: do
        let run = runFixture BellmanFord 0 (Just 3) weightedGraphText
        prResult run @?= PathFound [0, 2, 1, 3] 4,
      "BFS sin camino" ~: do
        let run = runFixture BFS 0 (Just 3) disconnectedGraphText
        prResult run @?= NoPath,
      "sin TARGET devuelve InputError en BFS" ~: do
        let run = runFixture BFS 0 Nothing simpleGraphText
        prResult run @?= InputError MissingTarget,
      "validatePathTarget rechaza BFS sin destino" ~: do
        case validatePathTarget BFS Nothing of
          Left MissingPathTarget -> return ()
          other -> assertFailure (show other),
      "validatePathTarget acepta PageRank sin destino" ~: do
        validatePathTarget PageRank Nothing @?= Right (),
      "Bellman-Ford rechaza grafo sin pesos" ~: do
        let graph = parseFixture simpleGraphText
        case resolveAlgorithm graph BellmanFord of
          Left WeightedGraphRequired -> return ()
          _ -> assertFailure "esperaba WeightedGraphRequired",
      "componentes conexas del origen" ~: do
        let run = runFixture ConnectedComponents 0 Nothing simpleGraphText
        prResult run @?= ComponentFound 0 [0, 1, 2, 3, 4],
      "componentes conexas en grafo desconectado" ~: do
        let run = runFixture ConnectedComponents 0 Nothing disconnectedGraphText
        prResult run @?= ComponentFound 0 [0, 1],
      "PageRank produce ranking por nodo" ~: do
        let run = runFixture PageRank 0 Nothing pageRankGraphText
        case prResult run of
          Rankings pairs -> length pairs @?= 4
          other -> assertFailure (show other),
      "label propagation produce etiquetas" ~: do
        let run = runFixture LabelPropagation 0 Nothing simpleGraphText
        case prResult run of
          NodeLabels pairs -> length pairs @?= 5
          other -> assertFailure (show other),
      "motor secuencial y concurrente coinciden (BFS)" ~: do
        let graph = parseFixture simpleGraphText
            spec = resolveFixture BFS graph
            cfg = mkRunConfigFor graph 0 (Just 4) BFS 4
            sequential = runSequential cfg spec
        concurrent <- runPregel cfg spec
        prResult sequential @?= prResult concurrent,
      "threads=1 concurrente coincide con secuencial (Bellman-Ford)" ~: do
        let graph = parseFixture weightedGraphText
            spec = resolveFixture BellmanFord graph
            cfg = mkRunConfigFor graph 0 (Just 3) BellmanFord 1
            sequential = runSequential cfg spec
        concurrent <- runPregel cfg spec
        prResult sequential @?= prResult concurrent
    ]
