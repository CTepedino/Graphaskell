module Fixtures
  ( FixtureError (..),
    simpleGraphText,
    weightedGraphText,
    disconnectedGraphText,
    pageRankGraphText,
    parseFixtureEither,
    parseFixture,
    runFixtureEither,
  )
where

import Algorithm.Spec (SomeAlgorithmSpec (..), resolveAlgorithm)
import Algorithm.Types (AlgorithmSpec (..))
import Graph.ParseError (ParseError)
import Graph.Parser (parseGraphFile)
import Algorithm.Name (Algorithm (..))
import Graph.Types (NodeId, ValidGraph, nodeCount)
import Pregel.Error (PregelError)
import SomePregelRun (SomePregelRun (..))
import Pregel.Types (RunConfig (..), mkRunConfig)
import SequentialEngine (runPregelSequential)

data FixtureError
  = FixtureParse ParseError
  | FixtureRun PregelError
  deriving Show

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

parseFixtureEither :: String -> Either ParseError ValidGraph
parseFixtureEither =
  parseGraphFile

parseFixture :: String -> Either FixtureError ValidGraph
parseFixture text =
  first FixtureParse (parseFixtureEither text)
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
  case resolveAlgorithm algorithm of
    SomeAlgorithmSpec spec -> do
      run <- first FixtureRun (runPregelSequential (mkRunConfigFor spec graph source target 1) spec)
      pure (SomePregelRun run)
  where
    first f (Left err) = Left (f err)
    first _ (Right value) = Right value

mkRunConfigFor ::
  AlgorithmSpec state msg log ->
  ValidGraph ->
  NodeId ->
  Maybe NodeId ->
  Int ->
  RunConfig
mkRunConfigFor spec graph source target threads =
  mkRunConfig
    graph
    (Just source)
    target
    threads
    (specMaxSupersteps spec (nodeCount graph))
    False
