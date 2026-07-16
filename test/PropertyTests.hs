module PropertyTests (propertyTests) where

import Algorithm.BFS (bfsSpec)
import Algorithm.BellmanFord (bellmanFordSpec)
import Algorithm.Common (reconstructPath, tryImproveDistance, tryRelabel)
import Algorithm.LabelPropagation (labelPropagationSpec, labelPropagationStable)
import Algorithm.PageRank (pageRankSpec)
import PageRankOracle (pageRankReference)
import Algorithm.Result (Result (..))
import Algorithm.Spec (SomeAlgorithmSpec (..), resolveAlgorithm)
import Algorithm.State (LabelState (..), PathState (..), emptyPathState)
import Algorithm.Types (AlgorithmSpec (..))
import Data.List (nub, nubBy)
import qualified Data.Map.Strict as Map
import Fixtures
  ( disconnectedGraphText,
    pageRankGraphText,
    parseFixtureEither,
    simpleGraphText,
    weightedGraphText,
  )
import Algorithm.Name (Algorithm (..))
import Graph.Parser (parseGraphFile)
import Graph.Types
  ( Distance (..),
    Edge (..),
    NodeId (..),
    ValidGraph,
    Weight (..),
    buildGraph,
    defaultEdgeWeight,
    nodeCount,
    unDistance,
  )
import Pregel.Types (PregelRun (..), RunConfig (..), mkRunConfig)
import SequentialEngine (runPregelSequential)
import Test.HUnit (Test (..), (~:), assertFailure)
import Test.QuickCheck
  ( Gen,
    Property,
    Testable,
    choose,
    forAll,
    listOf,
    property,
    stdArgs,
    vectorOf,
    (===),
  )
import Test.QuickCheck.Monadic qualified as QM (assert, monadicIO, run)
import Test.QuickCheck.Test qualified as QC
import TestSupport
  ( enginesAgree,
    rankingsApprox,
    shortestHops,
    shortestWeightedDistance,
    labelPropagationExpected,
    nodeLabelsMatch,
    validPath,
  )

propertyTests :: Test
propertyTests =
  TestList
    [ "prop: sequential engine is deterministic on CC" ~: check prop_sequentialDeterministic,
      "prop: sequential engine is deterministic on all algorithms" ~:
        check prop_sequentialDeterministicAll,
      "prop: BFS on reachable pairs finds optimal hop path" ~:
        check prop_bfsOptimalHopPath,
      "prop: Bellman-Ford on reachable pairs finds optimal weighted path" ~:
        check prop_bellmanFordOptimalPath,
      "prop: PageRank converges to expected rankings on generated graphs" ~:
        check prop_pageRankConvergesToExpected,
      "prop: label propagation converges to expected labels on fixture graph" ~:
        check prop_labelPropagationConvergesToExpected,
      "prop: label propagation reaches a stable labeling on connected graphs" ~:
        check prop_labelPropagationStableOnConnected,
      "prop: sequential and concurrent engines agree on all fixture algorithms" ~:
        check prop_enginesAgreeAll,
      "prop: sequential and concurrent engines agree on generated BFS graphs" ~:
        check prop_bfsEnginesAgree,
      "prop: sequential and concurrent engines agree on generated Bellman-Ford graphs" ~:
        check prop_bellmanFordEnginesAgree,
      "prop: tryImproveDistance never worsens distance" ~:
        check prop_tryImproveDistanceNeverWorsens,
      "prop: tryRelabel is idempotent on equal label" ~:
        check prop_tryRelabelIdempotent,
      "prop: reconstructPath follows predecessor links" ~:
        check prop_reconstructPathFollowsPredecessors,
      "prop: all fixture graphs parse" ~: check prop_allFixtureGraphsParse,
      "prop: parsed graph node count matches NODES directive" ~:
        check prop_parsedNodeCountMatchesDirective,
      "prop: algorithm max supersteps is positive" ~: check prop_maxSuperstepsPositive
    ]

check :: Testable prop => prop -> IO ()
check prop = do
  result <- QC.quickCheckWithResult stdArgs prop
  case result of
    QC.Success {} ->
      pure ()
    other ->
      assertFailure (show other)

prop_sequentialDeterministic :: Property
prop_sequentialDeterministic =
  case parseFixtureEither simpleGraphText of
    Right graph ->
      case resolveAlgorithm ConnectedComponents of
        SomeAlgorithmSpec spec ->
          property $
            let cfg =
                  mkRunConfig
                    graph
                    Nothing
                    Nothing
                    1
                    (specMaxSupersteps spec (nodeCount graph))
                    False
             in case (runPregelSequential cfg spec, runPregelSequential cfg spec) of
                  (Right first, Right second) ->
                    prResult first === prResult second
                  _ ->
                    property False
    _ ->
      property False

prop_sequentialDeterministicAll :: Property
prop_sequentialDeterministicAll =
  property $
    all deterministicCase algorithmCases

data AlgorithmCase = AlgorithmCase
  { acAlgorithm :: Algorithm,
    acGraphText :: String,
    acSource :: Maybe NodeId,
    acTarget :: Maybe NodeId,
    acThreads :: Int
  }

algorithmCases :: [AlgorithmCase]
algorithmCases =
  [ AlgorithmCase BFS simpleGraphText (Just (NodeId 0)) (Just (NodeId 4)) 4,
    AlgorithmCase BellmanFord weightedGraphText (Just (NodeId 0)) (Just (NodeId 3)) 1,
    AlgorithmCase ConnectedComponents disconnectedGraphText Nothing Nothing 2,
    AlgorithmCase PageRank pageRankGraphText Nothing Nothing 2,
    AlgorithmCase LabelPropagation simpleGraphText Nothing Nothing 2
  ]

deterministicCase :: AlgorithmCase -> Bool
deterministicCase AlgorithmCase {acAlgorithm, acGraphText, acSource, acTarget, acThreads} =
  case parseFixtureEither acGraphText of
    Right graph ->
      case resolveAlgorithm acAlgorithm of
        SomeAlgorithmSpec spec ->
          let cfg =
                mkRunConfig
                  graph
                  acSource
                  acTarget
                  acThreads
                  (specMaxSupersteps spec (nodeCount graph))
                  False
           in case (runPregelSequential cfg spec, runPregelSequential cfg spec) of
                (Right first, Right second) ->
                  prResult first == prResult second
                _ ->
                  False
    _ ->
      False

retryWhileNothing :: Gen (Maybe a) -> Gen a
retryWhileNothing gen = do
  mx <- gen
  case mx of
    Just value ->
      pure value
    Nothing ->
      retryWhileNothing gen

genReachableGraphCandidate :: Gen (Maybe (ValidGraph, NodeId, NodeId))
genReachableGraphCandidate = do
  nodeTotal <- choose (2, 5)
  target <- choose (1, nodeTotal - 1)
  extraCount <- choose (0, nodeTotal * 2)
  extras <-
    vectorOf extraCount $ do
      from <- choose (0, nodeTotal - 1)
      to <- choose (0, nodeTotal - 1)
      pure (from, to)
  let spine = zip [0 .. nodeTotal - 2] [1 .. nodeTotal - 1]
      pairs = nub (spine ++ [(from, to) | (from, to) <- extras, from /= to])
      edges = [Edge (NodeId from) (NodeId to) defaultEdgeWeight | (from, to) <- pairs]
  pure $
    case buildGraph nodeTotal edges of
      Right graph ->
        Just (graph, NodeId 0, NodeId target)
      Left _ ->
        Nothing

genReachableGraph :: Gen (ValidGraph, NodeId, NodeId)
genReachableGraph =
  retryWhileNothing genReachableGraphCandidate

genWeightedReachableGraphCandidate :: Gen (Maybe (ValidGraph, NodeId, NodeId))
genWeightedReachableGraphCandidate = do
  nodeTotal <- choose (2, 5)
  target <- choose (1, nodeTotal - 1)
  extraCount <- choose (0, nodeTotal * 2)
  extras <-
    vectorOf extraCount $ do
      from <- choose (0, nodeTotal - 1)
      to <- choose (0, nodeTotal - 1)
      weight <- choose (1, 9)
      pure (from, to, weight)
  let spine =
        [ (from, to, 1)
          | (from, to) <- zip [0 .. nodeTotal - 2] [1 .. nodeTotal - 1]
        ]
      pairs =
        nubBy
          (\(fromA, toA, _) (fromB, toB, _) -> fromA == fromB && toA == toB)
          ( spine
              ++ [ (from, to, weight)
                   | (from, to, weight) <- extras,
                     from /= to
                 ]
          )
      edges =
        [ Edge (NodeId from) (NodeId to) (Weight weight)
          | (from, to, weight) <- pairs
        ]
  pure $
    case buildGraph nodeTotal edges of
      Right graph ->
        Just (graph, NodeId 0, NodeId target)
      Left _ ->
        Nothing

genWeightedReachableGraph :: Gen (ValidGraph, NodeId, NodeId)
genWeightedReachableGraph =
  retryWhileNothing genWeightedReachableGraphCandidate

runBfsSequential :: ValidGraph -> NodeId -> NodeId -> Maybe Result
runBfsSequential graph source target =
  case runPregelSequential (mkPathConfig bfsSpec graph source target) bfsSpec of
    Right run ->
      Just (prResult run)
    Left _ ->
      Nothing

runBellmanFordSequential :: ValidGraph -> NodeId -> NodeId -> Maybe Result
runBellmanFordSequential graph source target =
  case
    runPregelSequential
      (mkPathConfig bellmanFordSpec graph source target)
      bellmanFordSpec
  of
    Right run ->
      Just (prResult run)
    Left _ ->
      Nothing

runPageRankSequential :: ValidGraph -> Maybe Result
runPageRankSequential graph =
  let n = nodeCount graph
      maxSteps = max 200 (n * n * n * 10)
   in case
        runPregelSequential
          ( mkRunConfig
              graph
              Nothing
              Nothing
              1
              maxSteps
              False
          )
          pageRankSpec
      of
        Right run ->
          Just (prResult run)
        Left _ ->
          Nothing

runLabelPropagationSequential :: ValidGraph -> Maybe Result
runLabelPropagationSequential graph =
  case
    runPregelSequential
      ( mkRunConfig
          graph
          Nothing
          Nothing
          1
          (specMaxSupersteps labelPropagationSpec (nodeCount graph))
          False
      )
      labelPropagationSpec
  of
    Right run ->
      Just (prResult run)
    Left _ ->
      Nothing

mkPathConfig ::
  AlgorithmSpec state msg log ->
  ValidGraph ->
  NodeId ->
  NodeId ->
  RunConfig
mkPathConfig spec graph source target =
  mkRunConfig
    graph
    (Just source)
    (Just target)
    1
    (max (specMaxSupersteps spec (nodeCount graph)) (nodeCount graph * nodeCount graph))
    False

prop_bfsOptimalHopPath :: Property
prop_bfsOptimalHopPath =
  forAll genReachableGraph $ \(graph, source, target) ->
    case runBfsSequential graph source target of
      Just (PathFound path dist) ->
        case shortestHops graph source target of
          Just minDist ->
            unDistance dist == minDist
              && unDistance dist + 1 == length path
              && validPath graph path
          Nothing ->
            False
      Just NoPath ->
        shortestHops graph source target == Nothing
      _ ->
        False

prop_bellmanFordOptimalPath :: Property
prop_bellmanFordOptimalPath =
  forAll genWeightedReachableGraph $ \(graph, source, target) ->
    case runBellmanFordSequential graph source target of
      Just (PathFound path dist) ->
        case shortestWeightedDistance graph source target of
          Just minDist ->
            dist == minDist && validPath graph path
          Nothing ->
            False
      Just NoPath ->
        shortestWeightedDistance graph source target == Nothing
      _ ->
        False

prop_pageRankConvergesToExpected :: Property
prop_pageRankConvergesToExpected =
  forAll genReachableGraph $ \(graph, _, _) ->
    case runPageRankSequential graph of
      Just result ->
        rankingsApprox 1e-6 (pageRankReference graph) result
          && case result of
            Rankings actual ->
              abs (sum [rank | (_, rank) <- actual] - 1.0) <= 1e-6
            _ ->
              False
      _ ->
        False

prop_labelPropagationConvergesToExpected :: Property
prop_labelPropagationConvergesToExpected =
  property $
    case parseFixtureEither simpleGraphText of
      Right graph ->
        case runLabelPropagationSequential graph of
          Just result ->
            nodeLabelsMatch labelPropagationExpected result
          _ ->
            False
      _ ->
        False

prop_labelPropagationStableOnConnected :: Property
prop_labelPropagationStableOnConnected =
  forAll genReachableGraph $ \(graph, _, _) ->
    case runLabelPropagationSequential graph of
      Just (NodeLabels pairs) ->
        labelPropagationStable graph pairs
      _ ->
        False

prop_enginesAgreeAll :: Property
prop_enginesAgreeAll =
  QM.monadicIO $
    mapM_
      ( \case' -> do
          ok <- QM.run (enginesAgreeCase case')
          QM.assert ok
      )
      algorithmCases

enginesAgreeCase :: AlgorithmCase -> IO Bool
enginesAgreeCase AlgorithmCase {acAlgorithm, acGraphText, acSource, acTarget, acThreads} =
  case parseFixtureEither acGraphText of
    Right graph ->
      case resolveAlgorithm acAlgorithm of
        SomeAlgorithmSpec spec ->
          enginesAgree
            ( mkRunConfig
                graph
                acSource
                acTarget
                acThreads
                (specMaxSupersteps spec (nodeCount graph))
                False
            )
            spec
    _ ->
      pure False

prop_bfsEnginesAgree :: Property
prop_bfsEnginesAgree =
  forAll genReachableGraph $ \(graph, source, target) ->
    QM.monadicIO $ do
      ok <- QM.run $ enginesAgree (mkPathConfig bfsSpec graph source target) bfsSpec
      QM.assert ok

prop_bellmanFordEnginesAgree :: Property
prop_bellmanFordEnginesAgree =
  forAll genWeightedReachableGraph $ \(graph, source, target) ->
    QM.monadicIO $ do
      ok <- QM.run $ enginesAgree (mkPathConfig bellmanFordSpec graph source target) bellmanFordSpec
      QM.assert ok

genDistance :: Gen Distance
genDistance =
  Distance <$> choose (0, 20)

genNodeId :: Gen NodeId
genNodeId =
  NodeId <$> choose (0, 10)

genCandidates :: Gen [(Distance, NodeId)]
genCandidates =
  listOf $
    (,) <$> genDistance <*> genNodeId

prop_tryImproveDistanceNeverWorsens :: Property
prop_tryImproveDistanceNeverWorsens =
  forAll genDistance $ \current ->
    forAll genCandidates $ \candidates ->
      forAll genNodeId $ \nodeId ->
        let state = emptyPathState {psDistance = Just current}
         in case tryImproveDistance nodeId candidates state of
              Nothing ->
                all ((>= current) . fst) candidates
              Just improved ->
                case psDistance improved of
                  Just newDist ->
                    newDist < current
                  Nothing ->
                    False

prop_tryRelabelIdempotent :: Property
prop_tryRelabelIdempotent =
  forAll genNodeId $ \nodeId ->
    forAll genNodeId $ \label ->
      tryRelabel nodeId label (LabelState label) === Nothing

prop_reconstructPathFollowsPredecessors :: Property
prop_reconstructPathFollowsPredecessors =
  forAll (choose (1, 4)) $ \pathLen ->
    let nodes = [NodeId n | n <- [0 .. pathLen]]
        states =
          Map.fromList
            [ ( node,
                PathState
                  (Just (Distance idx))
                  predecessor
              )
            | (idx, node) <- zip [0 .. pathLen] nodes,
              let predecessor =
                    if idx == 0
                      then Nothing
                      else Just (NodeId (idx - 1))
            ]
        source = NodeId 0
        target = NodeId pathLen
     in reconstructPath states target source === nodes

prop_allFixtureGraphsParse :: Property
prop_allFixtureGraphsParse =
  property $
    all
      ( isRight . parseFixtureEither
      )
      [ simpleGraphText,
        weightedGraphText,
        disconnectedGraphText,
        pageRankGraphText
      ]
  where
    isRight (Right _) =
      True
    isRight _ =
      False

prop_parsedNodeCountMatchesDirective :: Property
prop_parsedNodeCountMatchesDirective =
  forAll (choose (2, 8)) $ \nodeTotal ->
    let text =
          unlines
            [ "NODES " ++ show nodeTotal,
              "EDGES",
              "0 1"
            ]
     in case parseGraphFile text of
          Right graph -> nodeCount graph == nodeTotal
          Left _ -> False

prop_maxSuperstepsPositive :: Property
prop_maxSuperstepsPositive =
  forAll (choose (1, 100)) $ \nodeTotal ->
    all
      (positiveMaxSupersteps nodeTotal)
      [ resolveAlgorithm BFS,
        resolveAlgorithm BellmanFord,
        resolveAlgorithm PageRank,
        resolveAlgorithm ConnectedComponents,
        resolveAlgorithm LabelPropagation
      ]
  where
    positiveMaxSupersteps nodeTotal (SomeAlgorithmSpec spec) =
      specMaxSupersteps spec nodeTotal > 0
