module Fixtures
  ( simpleGraphText,
    weightedGraphText,
    disconnectedGraphText,
    noTargetGraphText,
    parseFixture,
    runFixture,
    mkRunConfigFor,
    resolveFixture,
  )
where

import Algorithm.Spec (resolveAlgorithm)
import Algorithm.Types (AlgorithmSpec)
import Graph.Parser (GraphFile (..), parseGraphFile)
import Graph.Types (Algorithm (..), Graph)
import Pregel.Types (RunConfig)
import Pregel.Engine (PregelRun, mkRunConfig, runSequential)

simpleGraphText :: String
simpleGraphText =
  unlines
    [ "NODES 5",
      "EDGES",
      "0 1",
      "0 2",
      "1 3",
      "2 3",
      "3 4",
      "SOURCE 0",
      "TARGET 4",
      "ALGORITHM BFS"
    ]

weightedGraphText :: String
weightedGraphText =
  unlines
    [ "NODES 4",
      "WEIGHTED",
      "EDGES",
      "0 1 4",
      "0 2 1",
      "2 1 2",
      "1 3 1",
      "2 3 5",
      "SOURCE 0",
      "TARGET 3",
      "ALGORITHM DIJKSTRA"
    ]

disconnectedGraphText :: String
disconnectedGraphText =
  unlines
    [ "NODES 4",
      "EDGES",
      "0 1",
      "2 3",
      "SOURCE 0",
      "TARGET 3",
      "ALGORITHM BFS"
    ]

noTargetGraphText :: String
noTargetGraphText =
  unlines
    [ "NODES 3",
      "EDGES",
      "0 1",
      "1 2",
      "SOURCE 0",
      "ALGORITHM BFS"
    ]

parseFixture :: String -> GraphFile
parseFixture text =
  case parseGraphFile text of
    Left parseError -> error ("parseFixture: " ++ show parseError)
    Right graphFile -> graphFile

resolveFixture :: Algorithm -> Graph -> AlgorithmSpec
resolveFixture algorithm graph =
  case resolveAlgorithm graph algorithm of
    Left algorithmError -> error ("resolveFixture: " ++ show algorithmError)
    Right spec -> spec

mkRunConfigFor :: GraphFile -> Algorithm -> Int -> RunConfig
mkRunConfigFor graphFile algorithm threads =
  mkRunConfig (graphFile {gfAlgorithm = algorithm}) threads

runFixture :: Algorithm -> String -> PregelRun
runFixture algorithm text =
  let graphFile = parseFixture text
      spec = resolveFixture algorithm (gfGraph graphFile)
      cfg = mkRunConfigFor graphFile algorithm 1
   in runSequential cfg spec
