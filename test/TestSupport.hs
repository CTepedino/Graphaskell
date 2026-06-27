module TestSupport
  ( assertComponentsListed,
    assertEnginesAgree,
    assertRankingsApprox,
    assertValidBfsPath,
    examplesGraphPaths,
    labelPropagationExpected,
    pageRankExpected,
    readExampleGraph,
  )
where

import Algorithm.Types (AlgorithmSpec)
import Data.List (isInfixOf)
import Graph.Parser (parseGraphFile)
import Graph.Types (Graph, NodeId, neighbors)
import Pregel.Engine (PregelRun (..), runPregel)
import Pregel.Types (RunConfig, Result (..))
import SequentialEngine (runSequential)
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

assertEnginesAgree :: RunConfig -> AlgorithmSpec -> IO Assertion
assertEnginesAgree cfg spec = do
  let sequential = runSequential cfg spec
  concurrent <- runPregel cfg spec
  pure (prResult sequential @?= prResult concurrent)

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
