module AlgorithmTests (algorithmTests) where

import Algorithm.Error (AlgorithmError (..))
import Algorithm.Spec (resolveAlgorithm)
import Fixtures
  ( disconnectedGraphText,
    mkRunConfigFor,
    noTargetGraphText,
    parseFixture,
    resolveFixture,
    runFixture,
    simpleGraphText,
    weightedGraphText,
  )
import Graph.Parser (GraphFile (..))
import Graph.Types (Algorithm (..))
import Pregel.Engine (PregelRun (..), runPregel, runSequential)
import Pregel.Types (InputError (..), Result (..))
import Test.HUnit

algorithmTests :: Test
algorithmTests =
  TestList
    [ "BFS encuentra camino minimo en saltos" ~: do
        let run = runFixture BFS simpleGraphText
        prResult run @?= PathFound [0, 2, 3, 4] 3,
      "DFS encuentra un camino valido" ~: do
        let run = runFixture DFS simpleGraphText
        prResult run @?= PathFound [0, 1, 3, 4] 3,
      "Dijkstra encuentra camino minimo ponderado" ~: do
        let run = runFixture Dijkstra weightedGraphText
        prResult run @?= PathFound [0, 2, 1, 3] 4,
      "BFS sin camino" ~: do
        let run = runFixture BFS disconnectedGraphText
        prResult run @?= NoPath,
      "sin TARGET devuelve InputError" ~: do
        let run = runFixture BFS noTargetGraphText
        prResult run @?= InputError MissingTarget,
      "Dijkstra rechaza grafo sin pesos" ~: do
        let graphFile = parseFixture simpleGraphText
        case resolveAlgorithm (gfGraph graphFile) Dijkstra of
          Left DijkstraRequiresWeightedGraph -> return ()
          _ -> assertFailure "esperaba DijkstraRequiresWeightedGraph",
      "motor secuencial y concurrente coinciden (BFS)" ~: do
        let graphFile = parseFixture simpleGraphText
            spec = resolveFixture BFS (gfGraph graphFile)
            cfg = mkRunConfigFor graphFile BFS 4
            sequential = runSequential cfg spec
        concurrent <- runPregel cfg spec
        prResult sequential @?= prResult concurrent,
      "threads=1 concurrente coincide con secuencial (Dijkstra)" ~: do
        let graphFile = parseFixture weightedGraphText
            spec = resolveFixture Dijkstra (gfGraph graphFile)
            cfg = mkRunConfigFor graphFile Dijkstra 1
            sequential = runSequential cfg spec
        concurrent <- runPregel cfg spec
        prResult sequential @?= prResult concurrent
    ]
