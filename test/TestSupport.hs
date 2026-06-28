module TestSupport
  ( assertComponentsListed,
    assertEnginesAgree,
    assertEnginesAgreeSome,
    assertRankingsApprox,
    assertValidBfsPath,
    examplesGraphPaths,
    labelPropagationExpected,
    pageRankExpected,
    readExampleGraph,
    validPath,
  )
where

import Algorithm.Log (MessageLog)
import Algorithm.Result (Result (..))
import Algorithm.Spec (SomeAlgorithmSpec (..))
import Algorithm.Types (AlgorithmSpec (..))
import Data.List (isInfixOf)
import Graph.Parser (parseGraphFile)
import Graph.Types (Graph, NodeId, neighbors, nodeCount)
import Pregel.Engine (runPregel)
import Pregel.Types
  ( PregelRun (..),
    RunConfig (..),
    mkRunConfig,
  )
import SequentialEngine (runPregelSequential)
import System.Directory (doesFileExist)
import Test.HUnit (Assertion, (@?=), assertBool, assertFailure)

pageRankExpected :: [(NodeId, Double)]
pageRankExpected =
  [ (0, 0.12002296283541904),
    (1, 0.13951951841010618),
    (2, 0.09679579532429514),
    (3, 0.09679579532429514)
  ]

labelPropagationExpected :: [(NodeId, NodeId)]
labelPropagationExpected =
  [ (0, 0),
    (1, 0),
    (2, 0),
    (3, 0),
    (4, 0)
  ]

assertEnginesAgree ::
  (MessageLog msg log, Eq log, Show log) =>
  RunConfig ->
  AlgorithmSpec state msg log ->
  IO Assertion
assertEnginesAgree cfg spec = do
  let sequential = runPregelSequential cfg spec
  concurrent <- runPregel cfg spec
  assertRunsAgree sequential concurrent

assertRunsAgree ::
  (Eq log, Show log, Show e) =>
  Either e (PregelRun log) ->
  Either e (PregelRun log) ->
  IO Assertion
assertRunsAgree sequential concurrent =
  case (sequential, concurrent) of
    (Right seqRun, Right concRun) -> do
      prResult seqRun @?= prResult concRun
      prSupersteps seqRun @?= prSupersteps concRun
      prMaxStepsReached seqRun @?= prMaxStepsReached concRun
      pure (prLogs seqRun @?= prLogs concRun)
    (Left err, _) ->
      assertFailure ("sequential engine failed: " ++ show err)
    (_, Left err) ->
      assertFailure ("concurrent engine failed: " ++ show err)

assertEnginesAgreeSome ::
  Graph ->
  NodeId ->
  Maybe NodeId ->
  Int ->
  SomeAlgorithmSpec ->
  IO Assertion
assertEnginesAgreeSome graph source target threads someSpec =
  case someSpec of
    SomeAlgorithmSpec spec ->
      assertEnginesAgree
        ( mkRunConfig
            graph
            source
            target
            threads
            (specMaxSupersteps spec (nodeCount graph))
            False
        )
        spec

assertRankingsApprox :: Double -> [(NodeId, Double)] -> Result -> Assertion
assertRankingsApprox epsilon expected result =
  case result of
    Rankings actual -> do
      map fst expected @?= map fst actual
      mapM_
        ( \(nodeId, expectedRank) ->
            case lookup nodeId actual of
              Nothing -> assertFailure ("missing rank for node " ++ show nodeId)
              Just actualRank
                | abs (actualRank - expectedRank) <= epsilon ->
                    pure ()
                | otherwise ->
                    assertFailure
                      ( "rank mismatch for node "
                          ++ show nodeId
                          ++ ": expected "
                          ++ show expectedRank
                          ++ ", got "
                          ++ show actualRank
                      )
        )
        expected
    other -> assertFailure ("expected Rankings, got " ++ show other)

assertValidBfsPath :: Graph -> NodeId -> NodeId -> Int -> Result -> Assertion
assertValidBfsPath graph source target expectedDist result =
  case result of
    PathFound path dist -> do
      dist @?= expectedDist
      assertBool "path is non-empty" (not (null path))
      head path @?= source
      last path @?= target
      assertBool "path follows edges" (validPath graph path)
    other -> assertFailure ("expected PathFound, got " ++ show other)

assertComponentsListed :: String -> String -> Assertion
assertComponentsListed needle haystack =
  assertBool ("output missing " ++ show needle) (needle `isInfixOf` haystack)

validPath :: Graph -> [NodeId] -> Bool
validPath _ [] = True
validPath _ [_] = True
validPath graph (from : to : rest) =
  any ((== to) . fst) (neighbors graph from) && validPath graph (to : rest)

examplesGraphPaths :: [FilePath]
examplesGraphPaths =
  [ "examples/grafo-simple.txt",
    "examples/grafo-weighted.txt",
    "examples/grafo-pagerank.txt"
  ]

readExampleGraph :: FilePath -> IO Graph
readExampleGraph path = do
  exists <- doesFileExist path
  if exists
    then do
      contents <- readFile path
      case parseGraphFile contents of
        Left err -> assertFailure ("failed to parse " ++ path ++ ": " ++ show err)
        Right graph -> pure graph
    else assertFailure ("missing example graph file: " ++ path)
