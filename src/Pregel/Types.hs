module Pregel.Types
  ( SuperstepLog (..),
    PregelRun (..),
    SomePregelRun (..),
    somePregelResult,
    RunConfig (..),
    VertexStepResult (..),
    VertexStates,
    MessageQueues,
  )
where

import Algorithm.Result (Result)
import Algorithm.Log (DescribeLogEntry)
import Data.Map.Strict (Map)
import Graph.Types

data SuperstepLog log = SuperstepLog
  { sslStep :: Int,
    sslActiveVertices :: Int,
    sslMessagesSent :: Int,
    sslEntries :: [log]
  }
  deriving (Eq, Show)

data PregelRun log = PregelRun
  { prSupersteps :: Int,
    prLogs :: [SuperstepLog log],
    prResult :: Result,
    prMaxStepsReached :: Bool
  }
  deriving (Eq, Show)

data SomePregelRun where
  SomePregelRun :: DescribeLogEntry log => PregelRun log -> SomePregelRun

somePregelResult :: SomePregelRun -> Result
somePregelResult (SomePregelRun run) =
  prResult run

data RunConfig = RunConfig
  { rcGraph :: Graph,
    rcSource :: NodeId,
    rcTarget :: Maybe NodeId,
    rcThreads :: Int,
    rcMaxSteps :: Int
  }
  deriving (Eq, Show)

data VertexStepResult state msg log = VertexStepResult
  { vsrState :: state,
    vsrOutgoing :: [(NodeId, msg)],
    vsrLogs :: [log]
  }
  deriving (Eq, Show)

type VertexStates state = Map NodeId state

type MessageQueues msg = Map NodeId [msg]
