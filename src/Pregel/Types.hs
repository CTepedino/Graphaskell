module Pregel.Types
  ( SuperstepLog (..),
    PregelRun (..),
    SomePregelRun (..),
    somePregelResult,
    PathRunConfig (..),
    RunConfig (..),
    VertexStepResult (..),
    VertexStates,
    MessageQueues,
    pathRunConfigToRunConfig,
    mkPathRunConfig,
    mkRunConfig,
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

data PathRunConfig = PathRunConfig
  { prcGraph :: Graph,
    prcSource :: NodeId,
    prcTarget :: NodeId,
    prcThreads :: Int,
    prcMaxSteps :: Int
  }
  deriving (Eq, Show)

data RunConfig = RunConfig
  { rcGraph :: Graph,
    rcSource :: NodeId,
    rcThreads :: Int,
    rcMaxSteps :: Int
  }
  deriving (Eq, Show)

pathRunConfigToRunConfig :: PathRunConfig -> RunConfig
pathRunConfigToRunConfig prc =
  RunConfig
    { rcGraph = prcGraph prc,
      rcSource = prcSource prc,
      rcThreads = prcThreads prc,
      rcMaxSteps = prcMaxSteps prc
    }

mkPathRunConfig ::
  Graph ->
  NodeId ->
  NodeId ->
  Int ->
  Int ->
  PathRunConfig
mkPathRunConfig graph source target threads maxSteps =
  PathRunConfig
    { prcGraph = graph,
      prcSource = source,
      prcTarget = target,
      prcThreads = threads,
      prcMaxSteps = maxSteps
    }

mkRunConfig :: Graph -> NodeId -> Int -> Int -> RunConfig
mkRunConfig graph source threads maxSteps =
  RunConfig
    { rcGraph = graph,
      rcSource = source,
      rcThreads = threads,
      rcMaxSteps = maxSteps
    }

data VertexStepResult state msg log = VertexStepResult
  { vsrState :: state,
    vsrOutgoing :: [(NodeId, msg)],
    vsrLogs :: [log]
  }
  deriving (Eq, Show)

type VertexStates state = Map NodeId state

type MessageQueues msg = Map NodeId [msg]
