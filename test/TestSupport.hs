module TestSupport
  ( assertComponentsListed,
    assertEnginesAgree,
    assertEnginesAgreeSome,
    assertRankingsApprox,
    assertValidBfsPath,
    enginesAgree,
    examplesGraphPaths,
    labelPropagationExpected,
    nodeLabelsMatch,
    pageRankExpected,
    pathSourceTarget,
    rankingsApprox,
    readExampleGraph,
    requireFixture,
    shortestHops,
    shortestWeightedDistance,
    validPath,
  )
where

import Algorithm.Log (MessageLog)
import Algorithm.Result (Result (..))
import Algorithm.Spec (SomeAlgorithmSpec (..))
import Algorithm.Types (AlgorithmSpec (..))
import Data.List (isInfixOf)
import Fixtures (FixtureError (..))
import Graph.Parser (parseGraphFile)
import Graph.Types
  ( Distance (..),
    NodeId (..),
    ValidGraph,
    distancePlusWeight,
    neighbors,
    nodeCount,
    zeroDistance,
  )
import Pregel.Engine (runPregel)
import Pregel.Types
  ( PregelRun (..),
    RunConfig (..),
    mkRunConfig,
  )
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import SequentialEngine (runPregelSequential)
import System.Directory (doesFileExist)
import Test.HUnit (Assertion, (@?=), assertBool, assertFailure)

pageRankExpected :: [(NodeId, Double)]
pageRankExpected =
  [ (NodeId 0, 0.12002296283541904),
    (NodeId 1, 0.13951951841010618),
    (NodeId 2, 0.09679579532429514),
    (NodeId 3, 0.09679579532429514)
  ]

labelPropagationExpected :: [(NodeId, NodeId)]
labelPropagationExpected =
  [ (NodeId 0, NodeId 0),
    (NodeId 1, NodeId 0),
    (NodeId 2, NodeId 0),
    (NodeId 3, NodeId 0),
    (NodeId 4, NodeId 0)
  ]

requireFixture :: Either FixtureError a -> IO a
requireFixture (Right value) =
  pure value
requireFixture (Left err) =
  assertFailure ("fixture failed: " ++ show err)

assertEnginesAgree ::
  (MessageLog msg log, Eq log, Show log, Eq state) =>
  RunConfig ->
  AlgorithmSpec state msg log ->
  IO Assertion
assertEnginesAgree cfg spec = do
  let sequential = runPregelSequential cfg spec
  concurrent <- runPregel cfg spec
  assertRunsAgree sequential concurrent

enginesAgree ::
  (MessageLog msg log, Eq log, Eq state) =>
  RunConfig ->
  AlgorithmSpec state msg log ->
  IO Bool
enginesAgree cfg spec = do
  let sequential = runPregelSequential cfg spec
  concurrent <- runPregel cfg spec
  pure $
    case (sequential, concurrent) of
      (Right seqRun, Right concRun) ->
        prResult seqRun == prResult concRun
          && prSupersteps seqRun == prSupersteps concRun
          && prMaxStepsReached seqRun == prMaxStepsReached concRun
          && prLogs seqRun == prLogs concRun
      _ ->
        False

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
  ValidGraph ->
  Maybe NodeId ->
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

rankingsApprox :: Double -> [(NodeId, Double)] -> Result -> Bool
rankingsApprox epsilon expected result =
  case result of
    Rankings actual ->
      map fst expected == map fst actual
        && all
          ( \(nodeId, expectedRank) ->
              case lookup nodeId actual of
                Just actualRank ->
                  abs (actualRank - expectedRank) <= epsilon
                Nothing ->
                  False
          )
          expected
    _ ->
      False

assertRankingsApprox :: Double -> [(NodeId, Double)] -> Result -> Assertion
assertRankingsApprox epsilon expected result =
  if rankingsApprox epsilon expected result
    then pure ()
    else assertFailure ("rankings differ from expected: " ++ show result)

nodeLabelsMatch :: [(NodeId, NodeId)] -> Result -> Bool
nodeLabelsMatch expected result =
  case result of
    NodeLabels actual ->
      expected == actual
    _ ->
      False

assertValidBfsPath :: ValidGraph -> NodeId -> NodeId -> Distance -> Result -> Assertion
assertValidBfsPath graph source target expectedDist result =
  case result of
    PathFound path dist -> do
      dist @?= expectedDist
      assertBool "path is non-empty" (not (null path))
      case pathSourceTarget path of
        Nothing ->
          assertFailure "path must have source and target endpoints"
        Just (pathSource, pathTarget) -> do
          pathSource @?= source
          pathTarget @?= target
          assertBool "path follows edges" (validPath graph path)
    other -> assertFailure ("expected PathFound, got " ++ show other)

pathSourceTarget :: [NodeId] -> Maybe (NodeId, NodeId)
pathSourceTarget [] =
  Nothing
pathSourceTarget path =
  case reverse path of
    [] ->
      Nothing
    target : rest ->
      case reverse rest of
        [] ->
          Just (target, target)
        source : _ ->
          Just (source, target)

assertComponentsListed :: String -> String -> Assertion
assertComponentsListed needle haystack =
  assertBool ("output missing " ++ show needle) (needle `isInfixOf` haystack)

validPath :: ValidGraph -> [NodeId] -> Bool
validPath _ [] =
  True
validPath _ [_] =
  True
validPath graph (from : to : rest) =
  any ((== to) . fst) (neighbors graph from) && validPath graph (to : rest)

shortestHops :: ValidGraph -> NodeId -> NodeId -> Maybe Int
shortestHops graph source target
  | source == target =
      Just 0
  | otherwise =
      go [(source, 0)] Set.empty
  where
    go [] _ =
      Nothing
    go ((node, dist) : queue) visited
      | node `Set.member` visited =
          go queue visited
      | node == target =
          Just dist
      | otherwise =
          let visited' = Set.insert node visited
              next =
                [ (to, dist + 1)
                  | (to, _) <- neighbors graph node,
                    to `Set.notMember` visited'
                ]
           in go (queue ++ next) visited'

shortestWeightedDistance :: ValidGraph -> NodeId -> NodeId -> Maybe Distance
shortestWeightedDistance graph source target
  | source == target =
      Just zeroDistance
  | otherwise =
      go (Map.singleton source zeroDistance) Set.empty
  where
    go dists visited
      | Map.null dists =
          Nothing
      | otherwise =
          case Map.minViewWithKey (Map.filterWithKey (\node _ -> node `Set.notMember` visited) dists) of
            Nothing ->
              Nothing
            Just ((node, dist), rest) ->
              if node == target
                then Just dist
                else
                  let visited' = Set.insert node visited
                      rest' =
                        foldr
                          (insertNeighbor node dist)
                          rest
                          (neighbors graph node)
                   in go rest' visited'
    insertNeighbor _from dist (to, weight) acc =
      let candidate = distancePlusWeight dist weight
       in case Map.lookup to acc of
            Just current | candidate >= current ->
              acc
            _ ->
              Map.insert to candidate acc

examplesGraphPaths :: [FilePath]
examplesGraphPaths =
  [ "examples/grafo-simple.txt",
    "examples/grafo-weighted.txt",
    "examples/grafo-pagerank.txt",
    "examples/grafo-disconnected.txt",
    "examples/grafo-componentes.txt",
    "examples/grafo-lp-comunidades.txt"
  ]

readExampleGraph :: FilePath -> IO ValidGraph
readExampleGraph path = do
  exists <- doesFileExist path
  if exists
    then do
      contents <- readFile path
      case parseGraphFile contents of
        Left err -> assertFailure ("failed to parse " ++ path ++ ": " ++ show err)
        Right graph -> pure graph
    else assertFailure ("missing example graph file: " ++ path)
