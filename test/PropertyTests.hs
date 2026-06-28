module PropertyTests (propertyTests) where

import Algorithm.BFS (bfsSpec)
import Algorithm.BellmanFord (bellmanFordSpec)
import Algorithm.Common (reconstructPath, tryImproveDistance, tryRelabel)
import Algorithm.PageRank (pageRankSpec)
import Algorithm.Result (Result (..))
import Algorithm.Spec (SomeAlgorithmSpec (..))
import Algorithm.State (LabelState (..), PathState (..), emptyPathState)
import Algorithm.Types (AlgorithmSpec (..))
import Data.List (nub, nubBy)
import qualified Data.Map.Strict as Map
import Fixtures
  ( disconnectedGraphText,
    pageRankGraphText,
    parseFixtureEither,
    resolveFixtureEither,
    simpleGraphText,
    weightedGraphText,
  )
import Graph.Parser (parseGraphFile)
import Graph.Types
  ( Algorithm (..),
    Distance (..),
    Edge (..),
    Graph,
    NodeId (..),
    Weight (..),
    buildGraph,
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
import Test.QuickCheck.Test qualified as QC
import TestSupport
  ( shortestHops,
    shortestWeightedDistance,
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
      "prop: PageRank rankings are non-negative on fixture graph" ~:
        check prop_pageRankNonNegative,
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
      case resolveFixtureEither ConnectedComponents graph of
        Right (SomeAlgorithmSpec spec) ->
          property $
            let cfg =
                  mkRunConfig
                    graph
                    (NodeId 0)
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
    _ ->
      property False

prop_sequentialDeterministicAll :: Property
prop_sequentialDeterministicAll =
  property $
    all deterministicCase algorithmCases

data AlgorithmCase = AlgorithmCase
  { acAlgorithm :: Algorithm,
    acGraphText :: String,
    acSource :: NodeId,
    acTarget :: Maybe NodeId
  }

algorithmCases :: [AlgorithmCase]
algorithmCases =
  [ AlgorithmCase BFS simpleGraphText (NodeId 0) (Just (NodeId 4)),
    AlgorithmCase BellmanFord weightedGraphText (NodeId 0) (Just (NodeId 3)),
    AlgorithmCase ConnectedComponents disconnectedGraphText (NodeId 0) Nothing,
    AlgorithmCase PageRank pageRankGraphText (NodeId 0) Nothing,
    AlgorithmCase LabelPropagation simpleGraphText (NodeId 0) Nothing
  ]

deterministicCase :: AlgorithmCase -> Bool
deterministicCase AlgorithmCase {acAlgorithm, acGraphText, acSource, acTarget} =
  case parseFixtureEither acGraphText of
    Right graph ->
      case resolveFixtureEither acAlgorithm graph of
        Right (SomeAlgorithmSpec spec) ->
          let cfg =
                mkRunConfig
                  graph
                  acSource
                  acTarget
                  1
                  (specMaxSupersteps spec (nodeCount graph))
                  False
           in case (runPregelSequential cfg spec, runPregelSequential cfg spec) of
                (Right first, Right second) ->
                  prResult first == prResult second
                _ ->
                  False
        _ ->
          False
    _ ->
      False

genReachableGraph :: Gen (Graph, NodeId, NodeId)
genReachableGraph = do
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
      edges = [Edge (NodeId from) (NodeId to) Nothing | (from, to) <- pairs]
  pure (buildGraph nodeTotal edges, NodeId 0, NodeId target)

genWeightedReachableGraph :: Gen (Graph, NodeId, NodeId)
genWeightedReachableGraph = do
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
        [ Edge (NodeId from) (NodeId to) (Just (Weight weight))
          | (from, to, weight) <- pairs
        ]
  pure (buildGraph nodeTotal edges, NodeId 0, NodeId target)

runBfsSequential :: Graph -> NodeId -> NodeId -> Maybe Result
runBfsSequential graph source target =
  case runPregelSequential (mkPathConfig bfsSpec graph source target) bfsSpec of
    Right run ->
      Just (prResult run)
    Left _ ->
      Nothing

runBellmanFordSequential :: Graph -> NodeId -> NodeId -> Maybe Result
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

runPageRankSequential :: Graph -> Maybe Result
runPageRankSequential graph =
  case
    runPregelSequential
      ( mkRunConfig
          graph
          (NodeId 0)
          Nothing
          1
          (specMaxSupersteps pageRankSpec (nodeCount graph))
          False
      )
      pageRankSpec
  of
    Right run ->
      Just (prResult run)
    Left _ ->
      Nothing

mkPathConfig ::
  AlgorithmSpec state msg log ->
  Graph ->
  NodeId ->
  NodeId ->
  RunConfig
mkPathConfig spec graph source target =
  mkRunConfig
    graph
    source
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

prop_pageRankNonNegative :: Property
prop_pageRankNonNegative =
  property $
    case parseFixtureEither pageRankGraphText of
      Right graph ->
        case runPageRankSequential graph of
          Just (Rankings pairs) ->
            all ((>= 0) . snd) pairs
          _ ->
            False
      _ ->
        False

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
    case parseFixtureEither simpleGraphText of
      Right simple ->
        case parseFixtureEither weightedGraphText of
          Right weighted ->
            all
              (positiveMaxSupersteps nodeTotal)
              [ resolveFixtureEither BFS simple,
                resolveFixtureEither BellmanFord weighted,
                resolveFixtureEither PageRank simple,
                resolveFixtureEither ConnectedComponents simple,
                resolveFixtureEither LabelPropagation simple
              ]
          _ ->
            False
      _ ->
        False
  where
    positiveMaxSupersteps nodeTotal (Right (SomeAlgorithmSpec spec)) =
      specMaxSupersteps spec nodeTotal > 0
    positiveMaxSupersteps _ _ =
      False
