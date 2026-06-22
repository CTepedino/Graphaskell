module TraceTests (traceTests) where

import Data.List (isInfixOf)
import Output.Trace (describeRun)
import Pregel.Engine (PregelRun (..))
import Pregel.Types
import Test.HUnit

traceTests :: Test
traceTests =
  TestList
    [ "describeRun sin verbose omite detalle de vertices" ~: do
        let output = describeRun False sampleRun
        assertBool "no muestra actualizaciones de vertice" $
          not ("vertice 0 actualizado" `isInfixOf` output),
      "describeRun verbose incluye detalle de vertices" ~: do
        let output = describeRun True sampleRun
        assertBool "muestra actualizaciones de vertice" $
          "vertice 0 actualizado" `isInfixOf` output,
      "describeRun advierte cuando se alcanza el limite de supersteps" ~: do
        let output = describeRun False maxStepsRun
        assertBool "muestra advertencia de limite" $
          "limite maximo de supersteps" `isInfixOf` output
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
