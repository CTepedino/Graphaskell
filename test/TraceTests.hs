module TraceTests (traceTests) where

import Algorithm.Log (PathLogEntry (..), RankLogEntry (..))
import Algorithm.Messages (DistanceMsg (..), RankMsg)
import Algorithm.Result (Result (..))
import Output.Result (describeResult)
import Data.List (isInfixOf)
import Fixtures (disconnectedGraphText, runFixture)
import Graph.Types (Algorithm (..), Distance (..), NodeId (..), zeroDistance)
import Output.Trace (describeRun)
import Output.Run (SomePregelRun (..))
import Pregel.Types (PregelRun (..), SuperstepLog (..))
import Test.HUnit
import TestSupport (assertComponentsListed, requireFixture)

traceTests :: Test
traceTests =
  TestList
    [ "describeRun without verbose omits vertex detail" ~: do
        let output = describeRun False sampleRun
        assertBool "does not show vertex updates" $
          not ("vertex 0 updated" `isInfixOf` output),
      "describeRun without verbose omits superstep headers" ~: do
        let output = describeRun False sampleRun
        assertBool "does not show superstep headers" $
          not ("Superstep" `isInfixOf` output),
      "describeRun with verbose includes vertex detail" ~: do
        let output = describeRun True sampleRun
        assertBool "shows vertex updates" $
          "vertex 0 updated" `isInfixOf` output,
      "describeRun warns when superstep limit is reached" ~: do
        let output = describeRun False maxStepsRun
        assertBool "shows limit warning" $
          "maximum superstep limit reached" `isInfixOf` output,
      "describeRun formats connected components" ~: do
        SomePregelRun run <-
          requireFixture (runFixture ConnectedComponents (NodeId 0) Nothing disconnectedGraphText)
        let output = describeRun False run
        assertComponentsListed "Result: connected components" output
        assertComponentsListed "component 0: [0,1]" output
        assertComponentsListed "component 2: [2,3]" output,
      "describeRun formats PageRank rankings" ~: do
        let output = describeRun False pageRankRun
        assertBool "shows PageRank header" $
          "Result: PageRank" `isInfixOf` output
        assertBool "shows node rank line" $
          "node 1:" `isInfixOf` output,
      "describeRun formats no path result" ~: do
        let output = describeRun False noPathRun
        assertBool "shows no path message" $
          "no path between source and target" `isInfixOf` output,
      "describeResult formats path found" ~: do
        let output = describeResult (PathFound [NodeId 0, NodeId 1, NodeId 2] (Distance 2))
        assertBool "shows path header" $
          "Result: path found" `isInfixOf` output
        assertBool "shows distance" $
          "Distance: 2" `isInfixOf` output
        assertBool "shows path" $
          "Path:     [0,1,2]" `isInfixOf` output,
      "describeResult formats no path" ~: do
        describeResult NoPath @?= "Result: no path between source and target",
      "describeResult formats connected components" ~: do
        let output = describeResult (Components [(NodeId 0, [NodeId 0, NodeId 1]), (NodeId 2, [NodeId 2, NodeId 3])])
        assertBool "shows components header" $
          "Result: connected components" `isInfixOf` output
        assertBool "shows first component" $
          "component 0: [0,1]" `isInfixOf` output
        assertBool "shows second component" $
          "component 2: [2,3]" `isInfixOf` output,
      "describeResult formats PageRank rankings" ~: do
        let output = describeResult (Rankings [(NodeId 0, 0.1), (NodeId 1, 0.2)])
        assertBool "shows PageRank header" $
          "Result: PageRank" `isInfixOf` output
        assertBool "shows node rank line" $
          "node 0: 0.1" `isInfixOf` output,
      "describeResult formats label propagation" ~: do
        let output = describeResult (NodeLabels [(NodeId 0, NodeId 0), (NodeId 1, NodeId 0)])
        assertBool "shows label propagation header" $
          "Result: label propagation" `isInfixOf` output
        assertBool "shows node label line" $
          "node 0 -> label 0" `isInfixOf` output
    ]

type SampleLog = PathLogEntry DistanceMsg

sampleRun :: PregelRun SampleLog
sampleRun =
  PregelRun
    { prSupersteps = 1,
      prLogs =
        [ SuperstepLog
            { sslStep = 0,
              sslActiveVertices = 1,
              sslMessagesSent = 1,
              sslEntries =
                [ PathDistanceUpdated (NodeId 0) (Distance 1),
                  PathMessageSent (NodeId 0) (NodeId 1) (DistanceMsg (NodeId 0) zeroDistance)
                ]
            }
        ],
      prResult = PathFound [NodeId 0, NodeId 1] (Distance 1),
      prMaxStepsReached = False
    }

maxStepsRun :: PregelRun SampleLog
maxStepsRun =
  sampleRun
    { prMaxStepsReached = True,
      prSupersteps = 25
    }

pageRankRun :: PregelRun (RankLogEntry RankMsg)
pageRankRun =
  PregelRun
    { prSupersteps = 1,
      prLogs = [],
      prResult = Rankings [(NodeId 0, 0.1), (NodeId 1, 0.2), (NodeId 2, 0.3), (NodeId 3, 0.4)],
      prMaxStepsReached = False
    }

noPathRun :: PregelRun SampleLog
noPathRun =
  sampleRun {prResult = NoPath}
