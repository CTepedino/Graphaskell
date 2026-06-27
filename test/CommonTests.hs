module CommonTests (commonTests) where

import Algorithm.Common
  ( VertexUpdate (..),
    bfsCandidates,
    extractPathResult,
    reconstructPath,
    tryImproveDistance,
  )
import Algorithm.Error (AlgorithmError (..))
import Algorithm.Messages (DistanceMsg (..))
import Algorithm.Result (Result (..))
import Algorithm.State (PathState (..), emptyPathState)
import qualified Data.Map.Strict as Map
import Graph.Types (Edge (..), Graph, buildGraph)
import Pregel.Types (RunConfig (..))
import Test.HUnit

commonTests :: Test
commonTests =
  TestList
    [ "tryImproveDistance accepts a better candidate" ~:
        case tryImproveDistance 1 [(2, 0)] (emptyPathState {psDistance = Just 5}) of
          Updated state _ ->
            psDistance state @?= Just 2
          other ->
            assertFailure ("expected Updated, got " ++ show other),
      "tryImproveDistance ignores a worse candidate" ~:
        tryImproveDistance 1 [(5, 0)] (emptyPathState {psDistance = Just 2})
          @?= Unchanged,
      "tryImproveDistance is unchanged with no candidates" ~:
        tryImproveDistance 1 [] emptyPathState @?= Unchanged,
      "bfsCandidates adds one hop per message" ~:
        bfsCandidates [DistanceMsg 0 1, DistanceMsg 2 3]
          @?= [(2, 0), (4, 2)],
      "reconstructPath follows predecessor chain" ~: do
        let states =
              Map.fromList
                [ (0, PathState (Just 0) Nothing),
                  (1, PathState (Just 1) (Just 0)),
                  (2, PathState (Just 2) (Just 1))
                ]
        reconstructPath states 2 0 @?= [0, 1, 2],
      "extractPathResult returns PathFound" ~: do
        let graph = sampleGraph
            cfg = sampleCfg graph (Just 2)
            states =
              Map.fromList
                [ (0, PathState (Just 0) Nothing),
                  (1, PathState (Just 1) (Just 0)),
                  (2, PathState (Just 2) (Just 1))
                ]
        extractPathResult states cfg @?= Right (PathFound [0, 1, 2] 2),
      "extractPathResult returns NoPath when target is unreachable" ~: do
        let graph = sampleGraph
            cfg = sampleCfg graph (Just 2)
            states =
              Map.fromList
                [ (0, PathState (Just 0) Nothing),
                  (1, emptyPathState),
                  (2, emptyPathState)
                ]
        extractPathResult states cfg @?= Right NoPath,
      "extractPathResult fails without target" ~: do
        let graph = sampleGraph
            cfg = sampleCfg graph Nothing
            states = Map.singleton 0 (PathState (Just 0) Nothing)
        extractPathResult states cfg @?= Left MissingPathTarget,
      "extractPathResult fails when target is missing from states" ~: do
        let graph = sampleGraph
            cfg = sampleCfg graph (Just 99)
            states = Map.singleton 0 (PathState (Just 0) Nothing)
        extractPathResult states cfg @?= Left (TargetNodeMissing 99)
    ]

sampleGraph :: Graph
sampleGraph =
  buildGraph
    3
    [ Edge 0 1 Nothing,
      Edge 1 2 Nothing
    ]

sampleCfg :: Graph -> Maybe Int -> RunConfig
sampleCfg graph target =
  RunConfig
    { rcGraph = graph,
      rcSource = 0,
      rcTarget = target,
      rcThreads = 1,
      rcMaxSteps = 100
    }
