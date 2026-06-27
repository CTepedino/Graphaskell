module PropertyTests (propertyTests) where

import Algorithm.Result (Result (..))
import Algorithm.Types (AlgorithmSpec (..), SomeAlgorithmSpec (..))
import Fixtures
  ( parseFixture,
    requireRight,
    resolveFixture,
    runFixture,
    simpleGraphText,
    weightedGraphText,
  )
import Graph.Parser (parseGraphFile)
import Graph.Types (Algorithm (..), nodeCount)
import Pregel.Engine (mkRunConfig)
import Pregel.Types (PregelRun (..), somePregelResult)
import SequentialEngine (runSequential)
import Test.HUnit (Test (..), (~:), assertFailure)
import Test.QuickCheck
  ( Property,
    Testable,
    choose,
    forAll,
    property,
    stdArgs,
    (===),
  )
import Test.QuickCheck.Test (Result (..), quickCheckWithResult)
import TestSupport (validPath)

propertyTests :: Test
propertyTests =
  TestList
    [ "prop: sequential engine is deterministic on CC" ~: check prop_sequentialDeterministic,
      "prop: BFS PathFound uses valid edges" ~: check prop_bfsPathUsesEdges,
      "prop: BFS hop distance matches path length" ~: check prop_bfsDistanceMatchesPathLength,
      "prop: parsed graph node count matches NODES directive" ~:
        check prop_parsedNodeCountMatchesDirective,
      "prop: algorithm max supersteps is positive" ~: check prop_maxSuperstepsPositive
    ]

check :: Testable prop => prop -> IO ()
check prop = do
  result <- quickCheckWithResult stdArgs prop
  case result of
    Success {} ->
      pure ()
    other ->
      assertFailure (show other)

prop_sequentialDeterministic :: Property
prop_sequentialDeterministic =
  case resolveFixture ConnectedComponents (parseFixture simpleGraphText) of
    SomeAlgorithmSpec spec ->
      let cfg = mkRunConfig (parseFixture simpleGraphText) 0 Nothing 1 spec
          first = requireRight (runSequential cfg spec)
          second = requireRight (runSequential cfg spec)
       in prResult first === prResult second

prop_bfsPathUsesEdges :: Property
prop_bfsPathUsesEdges =
  property $
    let graph = parseFixture simpleGraphText
        run = runFixture BFS 0 (Just 4) simpleGraphText
     in case somePregelResult run of
          PathFound path _ -> validPath graph path
          _ -> True

prop_bfsDistanceMatchesPathLength :: Property
prop_bfsDistanceMatchesPathLength =
  property $
    case somePregelResult (runFixture BFS 0 (Just 4) simpleGraphText) of
      PathFound path dist -> dist + 1 == length path
      _ -> True

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
                SomeAlgorithmSpec spec -> specMaxSupersteps spec nodeTotal > 0
          )
          specs
