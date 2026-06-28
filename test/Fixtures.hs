module Fixtures
  ( simpleGraphText,
    weightedGraphText,
    disconnectedGraphText,
    pageRankGraphText,
    parseFixture,
    parseFixtureEither,
    runFixture,
    runFixtureEither,
    resolveFixture,
    requireRight,
  )
where

import Algorithm.Error (AlgorithmError (..))
import Algorithm.Spec (SomeAlgorithmSpec (..), resolveAlgorithm)
import Algorithm.Types (GlobalAlgorithmSpec (..), PathAlgorithmSpec (..))
import Graph.ParseError (ParseError)
import Graph.Parser (parseGraphFile)
import Graph.Types (Algorithm (..), Graph, NodeId, nodeCount)
import Pregel.Error (PregelError (..))
import Output.Run (SomePregelRun (..))
import Pregel.Types (PathRunConfig (..), RunConfig (..), mkPathRunConfig, mkRunConfig)
import SequentialEngine (runGlobalSequential, runPathSequential)

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

requireRight :: Show e => Either e a -> a
requireRight (Right value) = value
requireRight (Left err) =
  error ("requireRight: " ++ show err)

parseFixtureEither :: String -> Either ParseError Graph
parseFixtureEither =
  parseGraphFile

parseFixture :: String -> Graph
parseFixture =
  requireRight . parseFixtureEither

resolveFixture :: Algorithm -> Graph -> SomeAlgorithmSpec
resolveFixture algorithm graph =
  requireRight (resolveAlgorithm graph algorithm)

runFixture ::
  Algorithm ->
  NodeId ->
  Maybe NodeId ->
  String ->
  SomePregelRun
runFixture algorithm source target text =
  requireRight (runFixtureEither algorithm source target text)

runFixtureEither ::
  Algorithm ->
  NodeId ->
  Maybe NodeId ->
  String ->
  Either PregelError SomePregelRun
runFixtureEither algorithm source target text =
  let graph = parseFixture text
   in case resolveFixture algorithm graph of
        SomePathAlgorithmSpec pathSpec ->
          case target of
            Nothing ->
              Left (ResultExtraction MissingPathTarget)
            Just targetNode ->
              fmap
                SomePregelRun
                ( runPathSequential
                    (mkPathRunConfigFor pathSpec graph source targetNode 1)
                    pathSpec
                )
        SomeGlobalAlgorithmSpec globalSpec ->
          fmap
            SomePregelRun
            ( runGlobalSequential
                (mkGlobalRunConfigFor globalSpec graph source 1)
                globalSpec
            )

mkPathRunConfigFor ::
  PathAlgorithmSpec ->
  Graph ->
  NodeId ->
  NodeId ->
  Int ->
  PathRunConfig
mkPathRunConfigFor pathSpec graph source target threads =
  mkPathRunConfig
    graph
    source
    target
    threads
    (psMaxSupersteps pathSpec (nodeCount graph))
    False

mkGlobalRunConfigFor ::
  GlobalAlgorithmSpec state msg log ->
  Graph ->
  NodeId ->
  Int ->
  RunConfig
mkGlobalRunConfigFor globalSpec graph source threads =
  mkRunConfig
    graph
    source
    threads
    (globalMaxSupersteps globalSpec (nodeCount graph))
    False
