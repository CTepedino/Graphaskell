module SuperstepTests (superstepTests) where

import Algorithm.BFS (bfsSpec)
import Algorithm.Log (PathLogEntry (..))
import Algorithm.Messages (DistanceMsg (..))
import Algorithm.State (PathState (..), emptyPathState)
import qualified Data.Map.Strict as Map
import Graph.Types (Edge (..), buildGraph)
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
              buildGraph
                3
                [ Edge 0 1 Nothing,
                  Edge 1 2 Nothing
                ]
            cfg =
              RunConfig
                { rcGraph = graph,
                  rcSource = 0,
                  rcTarget = Just 2,
                  rcThreads = 1,
                  rcMaxSteps = 100
                }
            contexts = buildVertexContexts graph
            states = initialVertexStates bfsSpec cfg graph
            messageFor 1 = [DistanceMsg 0 0]
            messageFor _ = []
        case processActiveVertices bfsSpec contexts states messageFor [1] of
          Right SuperstepResult {ssNewStates = newStates, ssOutgoing = outgoing, ssEntries = entries} -> do
            Map.lookup 1 newStates
              @?= Just (PathState (Just 1) (Just 0))
            Map.lookup 0 newStates @?= Map.lookup 0 states
            outgoing @?= [(2, DistanceMsg 1 1)]
            PathDistanceUpdated 1 1 `elem` entries @?= True
          Left err ->
            assertFailure ("expected successful superstep, got " ++ show err),
      "processActiveVertices leaves state unchanged without messages" ~: do
        let graph =
              buildGraph
                2
                [ Edge 0 1 Nothing
                ]
            cfg =
              RunConfig
                { rcGraph = graph,
                  rcSource = 0,
                  rcTarget = Just 1,
                  rcThreads = 1,
                  rcMaxSteps = 100
                }
            contexts = buildVertexContexts graph
            states = initialVertexStates bfsSpec cfg graph
            messageFor _ = []
        case processActiveVertices bfsSpec contexts states messageFor [1] of
          Right SuperstepResult {ssNewStates = newStates, ssOutgoing = outgoing, ssEntries = entries} -> do
            Map.lookup 1 newStates @?= Just emptyPathState
            outgoing @?= []
            entries @?= []
          Left err ->
            assertFailure ("expected successful superstep, got " ++ show err)
    ]
