module TraceTests (traceTests) where

import Data.List (isInfixOf)
import Fixtures (disconnectedGraphText, runFixture)
import Graph.Types (Algorithm (..))
import Output.Trace (describeRun)
import Pregel.Types
  ( DistanceMsg (..),
    LogEntry (..),
    PregelRun (..),
    RankMsg (..),
    Result (..),
    SomePregelRun (..),
    SuperstepLog (..),
  )
import Test.HUnit
import TestSupport (assertComponentsListed)

traceTests :: Test
traceTests =
  TestList
    [ "describeRun without verbose omits vertex detail" ~: do
        let output = describeRun False sampleRun
        assertBool "does not show vertex updates" $
          not ("vertex 0 updated" `isInfixOf` output),
      "describeRun with verbose includes vertex detail" ~: do
        let output = describeRun True sampleRun
        assertBool "shows vertex updates" $
          "vertex 0 updated" `isInfixOf` output,
      "describeRun warns when superstep limit is reached" ~: do
        let output = describeRun False maxStepsRun
        assertBool "shows limit warning" $
          "maximum superstep limit reached" `isInfixOf` output,
      "describeRun formats connected components" ~: do
        case runFixture ConnectedComponents 0 Nothing disconnectedGraphText of
          SomePregelRun run -> do
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
          "no path between source and target" `isInfixOf` output
    ]

sampleRun :: PregelRun DistanceMsg
sampleRun =
  PregelRun
    { prSupersteps = 1,
      prLogs =
        [ SuperstepLog
            { sslStep = 0,
              sslActiveVertices = 1,
              sslMessagesSent = 1,
              sslEntries =
                [ VertexUpdated 0 1,
                  MessageSent 0 1 (DistanceMsg 0 0)
                ]
            }
        ],
      prResult = PathFound [0, 1] 1,
      prMaxStepsReached = False
    }

maxStepsRun :: PregelRun DistanceMsg
maxStepsRun =
  sampleRun
    { prMaxStepsReached = True,
      prSupersteps = 25
    }

pageRankRun :: PregelRun RankMsg
pageRankRun =
  PregelRun
    { prSupersteps = 1,
      prLogs = [],
      prResult = Rankings [(0, 0.1), (1, 0.2), (2, 0.3), (3, 0.4)],
      prMaxStepsReached = False
    }

noPathRun :: PregelRun DistanceMsg
noPathRun =
  sampleRun {prResult = NoPath}
