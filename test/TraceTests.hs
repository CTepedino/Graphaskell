module TraceTests (traceTests) where

import Data.List (isInfixOf)
import Output.Trace (describeRun)
import Pregel.Engine (PregelRun (..))
import Pregel.Types
import Test.HUnit

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
          "maximum superstep limit reached" `isInfixOf` output
    ]

sampleRun :: PregelRun
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
                  MessageSent 0 1 (MsgDistance 0 0)
                ]
            }
        ],
      prResult = PathFound [0, 1] 1,
      prFinalStates = undefined,
      prMaxStepsReached = False
    }

maxStepsRun :: PregelRun
maxStepsRun =
  sampleRun
    { prMaxStepsReached = True,
      prSupersteps = 25
    }
