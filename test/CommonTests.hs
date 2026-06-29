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
import Graph.Types (Distance (..), Edge (..), GraphEndpoint (..), GraphError (..), NodeId (..), ValidGraph, buildGraph, defaultEdgeWeight)
import Pregel.Types (RunConfig (..))
import Test.HUnit

commonTests :: Test
commonTests =
  TestList
    [ "tryImproveDistance accepts a better candidate" ~:
        tryImproveDistance (NodeId 1) [(Distance 2, NodeId 0)] (emptyPathState {psDistance = Just (Distance 5)})
          @?= Just (PathState (Just (Distance 2)) (Just (NodeId 0))),
      "tryImproveDistance ignores a worse candidate" ~:
        tryImproveDistance (NodeId 1) [(Distance 5, NodeId 0)] (emptyPathState {psDistance = Just (Distance 2)})
          @?= Nothing,
      "tryImproveDistance is unchanged with no candidates" ~:
        tryImproveDistance (NodeId 1) [] emptyPathState @?= Nothing,
      "pathObserver records distance updates" ~:
        pathObserver
          (NodeId 1)
          emptyPathState
          (PathState (Just (Distance 2)) (Just (NodeId 0)))
          []
          @?= [PathDistanceUpdated (NodeId 1) (Distance 2)],
      "bfsCandidates adds one hop per message" ~:
        bfsCandidates [DistanceMsg (NodeId 0) (Distance 1), DistanceMsg (NodeId 2) (Distance 3)]
          @?= [(Distance 2, NodeId 0), (Distance 4, NodeId 2)],
      "reconstructPath follows predecessor chain" ~: do
        let states =
              Map.fromList
                [ (NodeId 0, PathState (Just (Distance 0)) Nothing),
                  (NodeId 1, PathState (Just (Distance 1)) (Just (NodeId 0))),
                  (NodeId 2, PathState (Just (Distance 2)) (Just (NodeId 1)))
                ]
        reconstructPath states (NodeId 2) (NodeId 0) @?= [NodeId 0, NodeId 1, NodeId 2],
      "extractPathResult returns PathFound" ~: do
        let graph = sampleGraph
            cfg = samplePathCfg graph (NodeId 2)
            states =
              Map.fromList
                [ (NodeId 0, PathState (Just (Distance 0)) Nothing),
                  (NodeId 1, PathState (Just (Distance 1)) (Just (NodeId 0))),
                  (NodeId 2, PathState (Just (Distance 2)) (Just (NodeId 1)))
                ]
        extractPathResult states cfg @?= Right (PathFound [NodeId 0, NodeId 1, NodeId 2] (Distance 2)),
      "extractPathResult returns NoPath when target is unreachable" ~: do
        let graph = sampleGraph
            cfg = samplePathCfg graph (NodeId 2)
            states =
              Map.fromList
                [ (NodeId 0, PathState (Just (Distance 0)) Nothing),
                  (NodeId 1, emptyPathState),
                  (NodeId 2, emptyPathState)
                ]
        extractPathResult states cfg @?= Right NoPath,
      "extractPathResult fails when target is missing from states" ~: do
        let graph = sampleGraph
            cfg = samplePathCfg graph (NodeId 99)
            states = Map.singleton (NodeId 0) (PathState (Just (Distance 0)) Nothing)
        extractPathResult states cfg @?= Left (TargetNodeMissing (NodeId 99)),
      "buildGraph rejects out-of-range edge endpoints" ~:
        buildGraph 3 [Edge (NodeId 0) (NodeId 5) defaultEdgeWeight]
          @?= Left (GraphBuildInvalidNodeId EdgeTo (NodeId 5) 2),
      "buildGraph rejects non-positive node counts" ~:
        buildGraph 0 [] @?= Left (GraphBuildInvalidNodeCount 0)
    ]

sampleGraph :: ValidGraph
sampleGraph =
  case
    buildGraph
      3
      [ Edge (NodeId 0) (NodeId 1) defaultEdgeWeight,
        Edge (NodeId 1) (NodeId 2) defaultEdgeWeight
      ]
  of
    Right graph -> graph
    Left err -> error ("sampleGraph construction failed: " ++ show err)

samplePathCfg :: ValidGraph -> NodeId -> RunConfig
samplePathCfg graph target =
  RunConfig
    { rcGraph = graph,
      rcSource = Just (NodeId 0),
      rcTarget = Just target,
      rcThreads = 1,
      rcMaxSteps = 100,
      rcTrace = False
    }
