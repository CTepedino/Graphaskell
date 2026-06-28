module CommonTests (commonTests) where

import Algorithm.Common
  ( bfsCandidates,
    extractPathResult,
    reconstructPath,
    tryImproveDistance,
  )
import Algorithm.Error (AlgorithmError (..))
import Algorithm.Messages (DistanceMsg (..))
import Algorithm.Log (PathLogEntry (..))
import Algorithm.Observability (pathObserver)
import Algorithm.Result (Result (..))
import Algorithm.State (PathState (..), emptyPathState)
import qualified Data.Map.Strict as Map
import Graph.Types (Edge (..), Graph, NodeId, buildGraph)
import Pregel.Types (RunConfig (..))
import Test.HUnit

commonTests :: Test
commonTests =
  TestList
    [ "tryImproveDistance accepts a better candidate" ~:
        tryImproveDistance 1 [(2, 0)] (emptyPathState {psDistance = Just 5})
          @?= Just (PathState (Just 2) (Just 0)),
      "tryImproveDistance ignores a worse candidate" ~:
        tryImproveDistance 1 [(5, 0)] (emptyPathState {psDistance = Just 2})
          @?= Nothing,
      "tryImproveDistance is unchanged with no candidates" ~:
        tryImproveDistance 1 [] emptyPathState @?= Nothing,
      "pathObserver records distance updates" ~:
        pathObserver
          1
          emptyPathState
          (PathState (Just 2) (Just 0))
          []
          @?= [PathDistanceUpdated 1 2],
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
            cfg = samplePathCfg graph 2
            states =
              Map.fromList
                [ (0, PathState (Just 0) Nothing),
                  (1, PathState (Just 1) (Just 0)),
                  (2, PathState (Just 2) (Just 1))
                ]
        extractPathResult states cfg @?= Right (PathFound [0, 1, 2] 2),
      "extractPathResult returns NoPath when target is unreachable" ~: do
        let graph = sampleGraph
            cfg = samplePathCfg graph 2
            states =
              Map.fromList
                [ (0, PathState (Just 0) Nothing),
                  (1, emptyPathState),
                  (2, emptyPathState)
                ]
        extractPathResult states cfg @?= Right NoPath,
      "extractPathResult fails when target is missing from states" ~: do
        let graph = sampleGraph
            cfg = samplePathCfg graph 99
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

samplePathCfg :: Graph -> NodeId -> RunConfig
samplePathCfg graph target =
  RunConfig
    { rcGraph = graph,
      rcSource = 0,
      rcTarget = Just target,
      rcThreads = 1,
      rcMaxSteps = 100,
      rcTrace = False
    }
