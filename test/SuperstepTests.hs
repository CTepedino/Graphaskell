module SuperstepTests (superstepTests) where

import Algorithm.BFS (bfsPathSpec)
import Algorithm.Log (PathLogEntry (..))
import Algorithm.Messages (DistanceMsg (..))
import Algorithm.State (PathState (..), emptyPathState)
import Algorithm.Types (pathRunSpec)
import qualified Data.Map.Strict as Map
import Graph.Types (Edge (..), buildGraph)
import Graph.VertexContext (buildVertexContexts)
import Pregel.Superstep
  ( SuperstepResult (..),
    initialVertexStates,
    processActiveVertices,
  )
import Pregel.Types (PathRunConfig (..), pathRunConfigToRunConfig)
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
            prc =
              PathRunConfig
                { prcGraph = graph,
                  prcSource = 0,
                  prcTarget = 2,
                  prcThreads = 1,
                  prcMaxSteps = 100,
                  prcTrace = True
                }
            spec = pathRunSpec bfsPathSpec prc
            cfg = pathRunConfigToRunConfig prc
            contexts = buildVertexContexts graph
            states = initialVertexStates spec cfg graph
            messageFor 1 = [DistanceMsg 0 0]
            messageFor _ = []
        case processActiveVertices True spec contexts states messageFor [1] of
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
            prc =
              PathRunConfig
                { prcGraph = graph,
                  prcSource = 0,
                  prcTarget = 1,
                  prcThreads = 1,
                  prcMaxSteps = 100,
                  prcTrace = False
                }
            spec = pathRunSpec bfsPathSpec prc
            cfg = pathRunConfigToRunConfig prc
            contexts = buildVertexContexts graph
            states = initialVertexStates spec cfg graph
            messageFor _ = []
        case processActiveVertices False spec contexts states messageFor [1] of
          Right SuperstepResult {ssNewStates = newStates, ssOutgoing = outgoing, ssEntries = entries} -> do
            Map.lookup 1 newStates @?= Just emptyPathState
            outgoing @?= []
            entries @?= []
          Left err ->
            assertFailure ("expected successful superstep, got " ++ show err)
    ]
