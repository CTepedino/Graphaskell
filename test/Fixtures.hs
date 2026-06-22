module Fixtures
  ( simpleGraphText,
    weightedGraphText,
    disconnectedGraphText,
    pageRankGraphText,
    parseFixture,
    runFixture,
    mkRunConfigFor,
    resolveFixture,
  )
where

import Algorithm.Spec (resolveAlgorithm)
import Algorithm.Types (AlgorithmSpec)
import Graph.Parser (parseGraphFile)
import Graph.Types (Algorithm (..), Graph, NodeId)
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
      "3 4"
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
      "2 3 5"
    ]

disconnectedGraphText :: String
disconnectedGraphText =
  unlines
    [ "NODES 4",
      "EDGES",
      "0 1",
      "2 3"
    ]

pageRankGraphText :: String
pageRankGraphText =
  unlines
    [ "NODES 4",
      "EDGES",
      "0 1",
      "1 2",
      "2 0",
      "1 3"
    ]

parseFixture :: String -> Graph
parseFixture text =
  case parseGraphFile text of
    Left parseError -> error ("parseFixture: " ++ show parseError)
    Right graph -> graph

resolveFixture :: Algorithm -> Graph -> AlgorithmSpec
resolveFixture algorithm graph =
  case resolveAlgorithm graph algorithm of
    Left algorithmError -> error ("resolveFixture: " ++ show algorithmError)
    Right spec -> spec

mkRunConfigFor ::
  Graph ->
  NodeId ->
  Maybe NodeId ->
  Algorithm ->
  Int ->
  RunConfig
mkRunConfigFor = mkRunConfig

runFixture ::
  Algorithm ->
  NodeId ->
  Maybe NodeId ->
  String ->
  PregelRun
runFixture algorithm source target text =
  let graph = parseFixture text
      spec = resolveFixture algorithm graph
      cfg = mkRunConfigFor graph source target algorithm 1
   in runSequential cfg spec
