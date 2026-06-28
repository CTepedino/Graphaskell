module PropertyTests (propertyTests) where

import Algorithm.BFS (bfsPathSpec)
import Algorithm.Result (Result (..))
import Algorithm.Spec (SomeAlgorithmSpec (..))
import Algorithm.Types (GlobalAlgorithmSpec (..), PathAlgorithmSpec (..))
import Data.List (nub)
import qualified Data.Set as Set
import Fixtures
  ( disconnectedGraphText,
    pageRankGraphText,
    parseFixture,
    requireRight,
    resolveFixture,
    simpleGraphText,
    weightedGraphText,
  )
import Graph.Parser (parseGraphFile)
import Graph.Types
  ( Algorithm (..),
    Edge (..),
    Graph,
    NodeId,
    buildGraph,
    neighbors,
    nodeCount,
  )
import Pregel.Types (PregelRun (..), mkPathRunConfig, mkRunConfig)
import SequentialEngine (runGlobalSequential, runPathSequential)
import Test.HUnit (Test (..), (~:), assertFailure)
import Test.QuickCheck
  ( Gen,
    Property,
    Testable,
    choose,
    forAll,
    property,
    stdArgs,
    vectorOf,
    (===),
  )
import Test.QuickCheck.Test qualified as QC
import TestSupport (validPath)

propertyTests :: Test
propertyTests =
  TestList
    [ "prop: sequential engine is deterministic on CC" ~: check prop_sequentialDeterministic,
      "prop: sequential engine is deterministic on all algorithms" ~:
        check prop_sequentialDeterministicAll,
      "prop: BFS on reachable pairs finds optimal hop path" ~:
        check prop_bfsOptimalHopPath,
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
  case resolveFixture ConnectedComponents (parseFixture simpleGraphText) of
    SomeGlobalAlgorithmSpec globalSpec ->
      property $
        let graph = parseFixture simpleGraphText
            cfg =
              mkRunConfig
                graph
                0
                1
                (globalMaxSupersteps globalSpec (nodeCount graph))
                False
            first = requireRight (runGlobalSequential cfg globalSpec)
            second = requireRight (runGlobalSequential cfg globalSpec)
         in prResult first === prResult second
    _ ->
      property False

prop_sequentialDeterministicAll :: Property
prop_sequentialDeterministicAll =
  property $
    all deterministicCase algorithmCases

data AlgorithmCase = AlgorithmCase
  { acAlgorithm :: Algorithm,
    acGraphText :: String,
    acSource :: Int,
    acTarget :: Maybe Int
  }

algorithmCases :: [AlgorithmCase]
algorithmCases =
  [ AlgorithmCase BFS simpleGraphText 0 (Just 4),
    AlgorithmCase BellmanFord weightedGraphText 0 (Just 3),
    AlgorithmCase ConnectedComponents disconnectedGraphText 0 Nothing,
    AlgorithmCase PageRank pageRankGraphText 0 Nothing,
    AlgorithmCase LabelPropagation simpleGraphText 0 Nothing
  ]

deterministicCase :: AlgorithmCase -> Bool
deterministicCase AlgorithmCase {acAlgorithm, acGraphText, acSource, acTarget} =
  let graph = parseFixture acGraphText
   in case resolveFixture acAlgorithm graph of
        SomePathAlgorithmSpec pathSpec ->
          case acTarget of
            Nothing -> False
            Just target ->
              let prc =
                    mkPathRunConfig
                      graph
                      acSource
                      target
                      1
                      (psMaxSupersteps pathSpec (nodeCount graph))
                      False
                  first = requireRight (runPathSequential prc pathSpec)
                  second = requireRight (runPathSequential prc pathSpec)
               in prResult first == prResult second
        SomeGlobalAlgorithmSpec globalSpec ->
          let cfg =
                mkRunConfig
                  graph
                  acSource
                  1
                  (globalMaxSupersteps globalSpec (nodeCount graph))
                  False
              first = requireRight (runGlobalSequential cfg globalSpec)
              second = requireRight (runGlobalSequential cfg globalSpec)
           in prResult first == prResult second

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
      edges = [Edge from to Nothing | (from, to) <- pairs]
  pure (buildGraph nodeTotal edges, 0, target)

shortestHops :: Graph -> NodeId -> NodeId -> Maybe Int
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

runBfsSequential :: Graph -> NodeId -> NodeId -> Result
runBfsSequential graph source target =
  prResult
    ( requireRight
        ( runPathSequential
            ( mkPathRunConfig
                graph
                source
                target
                1
                (psMaxSupersteps bfsPathSpec (nodeCount graph))
                False
            )
            bfsPathSpec
        )
    )

prop_bfsOptimalHopPath :: Property
prop_bfsOptimalHopPath =
  forAll genReachableGraph $ \(graph, source, target) ->
    case runBfsSequential graph source target of
      PathFound path dist ->
        case shortestHops graph source target of
          Just minDist ->
            dist == minDist
              && dist + 1 == length path
              && validPath graph path
          Nothing ->
            False
      NoPath ->
        shortestHops graph source target == Nothing
      _ ->
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
    let simple = parseFixture simpleGraphText
        weighted = parseFixture weightedGraphText
        specs =
          [ resolveFixture BFS simple,
            resolveFixture BellmanFord weighted,
            resolveFixture PageRank simple,
            resolveFixture ConnectedComponents simple,
            resolveFixture LabelPropagation simple
          ]
     in all
          ( \someSpec ->
              case someSpec of
                SomePathAlgorithmSpec pathSpec ->
                  psMaxSupersteps pathSpec nodeTotal > 0
                SomeGlobalAlgorithmSpec globalSpec ->
                  globalMaxSupersteps globalSpec nodeTotal > 0
          )
          specs
