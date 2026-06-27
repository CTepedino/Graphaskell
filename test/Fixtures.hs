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

import Algorithm.Spec (SomeAlgorithmSpec (..), resolveAlgorithm)
import Graph.ParseError (ParseError)
import Graph.Parser (parseGraphFile)
import Graph.Types (Algorithm (..), Graph, NodeId)
import Pregel.Engine (mkRunConfig)
import Pregel.Error (PregelError)
import Pregel.Types (RunConfig (..), SomePregelRun (..))
import SequentialEngine (runSomeSequential)

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
      spec = resolveFixture algorithm graph
      cfg = mkRunConfigForSpec graph source target 1 spec
   in runSomeSequential cfg spec

mkRunConfigForSpec ::
  Graph ->
  NodeId ->
  Maybe NodeId ->
  Int ->
  SomeAlgorithmSpec ->
  RunConfig
mkRunConfigForSpec graph source target threads (SomeAlgorithmSpec spec) =
  mkRunConfig graph source target threads spec
