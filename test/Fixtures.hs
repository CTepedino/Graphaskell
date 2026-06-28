module Fixtures
  ( FixtureError (..),
    simpleGraphText,
    weightedGraphText,
    disconnectedGraphText,
    pageRankGraphText,
    parseFixtureEither,
    parseFixture,
    resolveFixture,
    resolveFixtureEither,
    runFixture,
    runFixtureEither,
  )
where

import Algorithm.Error (AlgorithmError)
import Algorithm.Spec (SomeAlgorithmSpec (..), resolveAlgorithm)
import Algorithm.Types (AlgorithmSpec (..))
import Graph.ParseError (ParseError)
import Graph.Parser (parseGraphFile)
import Graph.Types (Algorithm (..), Graph, NodeId, nodeCount)
import Pregel.Error (PregelError)
import Output.Run (SomePregelRun (..))
import Pregel.Types (RunConfig (..), mkRunConfig)
import SequentialEngine (runPregelSequential)

data FixtureError
  = FixtureParse ParseError
  | FixtureResolve AlgorithmError
  | FixtureRun PregelError
  deriving (Eq, Show)

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

parseFixtureEither :: String -> Either ParseError Graph
parseFixtureEither =
  parseGraphFile

parseFixture :: String -> Either FixtureError Graph
parseFixture text =
  first FixtureParse (parseFixtureEither text)
  where
    first f (Left err) = Left (f err)
    first _ (Right value) = Right value

resolveFixtureEither :: Algorithm -> Graph -> Either AlgorithmError SomeAlgorithmSpec
resolveFixtureEither algorithm graph =
  resolveAlgorithm graph algorithm

resolveFixture :: Algorithm -> Graph -> Either FixtureError SomeAlgorithmSpec
resolveFixture algorithm graph =
  first FixtureResolve (resolveAlgorithm graph algorithm)
  where
    first f (Left err) = Left (f err)
    first _ (Right value) = Right value

runFixtureEither ::
  Algorithm ->
  NodeId ->
  Maybe NodeId ->
  String ->
  Either FixtureError SomePregelRun
runFixtureEither algorithm source target text = do
  graph <- parseFixture text
  SomeAlgorithmSpec spec <- resolveFixture algorithm graph
  run <- first FixtureRun (runPregelSequential (mkRunConfigFor spec graph source target 1) spec)
  pure (SomePregelRun run)
  where
    first f (Left err) = Left (f err)
    first _ (Right value) = Right value

runFixture ::
  Algorithm ->
  NodeId ->
  Maybe NodeId ->
  String ->
  Either FixtureError SomePregelRun
runFixture =
  runFixtureEither

mkRunConfigFor ::
  AlgorithmSpec state msg log ->
  Graph ->
  NodeId ->
  Maybe NodeId ->
  Int ->
  RunConfig
mkRunConfigFor spec graph source target threads =
  mkRunConfig
    graph
    source
    target
    threads
    (specMaxSupersteps spec (nodeCount graph))
    False
