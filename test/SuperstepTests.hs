module SuperstepTests (superstepTests) where

import Algorithm.BFS (bfsSpec)
import Algorithm.Log (PathLogEntry (..))
import Algorithm.Messages (DistanceMsg (..))
import Algorithm.State (PathState (..), emptyPathState)
import qualified Data.Map.Strict as Map
import Graph.Types (Distance (..), Edge (..), NodeId (..), ValidGraph, buildGraph, defaultEdgeWeight, zeroDistance)
import Graph.VertexContext (buildVertexContexts)
import Pregel.Superstep
  ( SuperstepResult (..),
    initialVertexStates,
    processActiveVertices,
  )
import Pregel.Types (RunConfig (..))
import Test.HUnit

superstepTests :: Test
superstepTests =
  TestList
    [ "processActiveVertices applies BFS update from bootstrap message" ~: do
        let graph =
              requireGraph
                3
                [ Edge (NodeId 0) (NodeId 1) defaultEdgeWeight,
                  Edge (NodeId 1) (NodeId 2) defaultEdgeWeight
                ]
            cfg =
              RunConfig
                { rcGraph = graph,
                  rcSource = Just (NodeId 0),
                  rcTarget = Just (NodeId 2),
                  rcThreads = 1,
                  rcMaxSteps = 100,
                  rcTrace = True
                }
            contexts = buildVertexContexts graph
            states = initialVertexStates bfsSpec cfg graph
            messageFor :: NodeId -> [DistanceMsg]
            messageFor (NodeId 1) = [DistanceMsg (NodeId 0) zeroDistance]
            messageFor _ = []
        case processActiveVertices True bfsSpec contexts states messageFor [NodeId 1] of
          Right SuperstepResult {ssNewStates = newStates, ssOutgoing = outgoing, ssEntries = entries} -> do
            Map.lookup (NodeId 1) newStates
              @?= Just (PathState (Just (Distance 1)) (Just (NodeId 0)))
            Map.lookup (NodeId 0) newStates @?= Map.lookup (NodeId 0) states
            outgoing @?= [(NodeId 2, DistanceMsg (NodeId 1) (Distance 1))]
            PathDistanceUpdated (NodeId 1) (Distance 1) `elem` entries @?= True
          Left err ->
            assertFailure ("expected successful superstep, got " ++ show err),
      "processActiveVertices leaves state unchanged without messages" ~: do
        let graph =
              requireGraph
                2
                [ Edge (NodeId 0) (NodeId 1) defaultEdgeWeight
                ]
            cfg =
              RunConfig
                { rcGraph = graph,
                  rcSource = Just (NodeId 0),
                  rcTarget = Just (NodeId 1),
                  rcThreads = 1,
                  rcMaxSteps = 100,
                  rcTrace = False
                }
            contexts = buildVertexContexts graph
            states = initialVertexStates bfsSpec cfg graph
            messageFor :: NodeId -> [DistanceMsg]
            messageFor _ = []
        case processActiveVertices False bfsSpec contexts states messageFor [NodeId 1] of
          Right SuperstepResult {ssNewStates = newStates, ssOutgoing = outgoing, ssEntries = entries} -> do
            Map.lookup (NodeId 1) newStates @?= Just emptyPathState
            outgoing @?= []
            entries @?= []
          Left err ->
            assertFailure ("expected successful superstep, got " ++ show err)
    ]

requireGraph :: Int -> [Edge] -> ValidGraph
requireGraph nodeTotal edges =
  case buildGraph nodeTotal edges of
    Right graph -> graph
    Left err -> error ("requireGraph failed: " ++ show err)
