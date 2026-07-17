module Pregel.Types
  ( SuperstepLog (..),
    PregelRun (..),
    RunConfig (..),
    VertexStepResult (..),
    VertexStates,
    MessageQueues,
    mkRunConfig,
  )
where

import Algorithm.Result (Result)
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
  deriving Eq

data RunConfig = RunConfig
  { rcGraph :: ValidGraph,
    rcSource :: Maybe NodeId,
    rcTarget :: Maybe NodeId,
    rcThreads :: Int,
    rcMaxSteps :: Int,
    rcTrace :: Bool
  }

mkRunConfig ::
  ValidGraph ->
  Maybe NodeId ->
  Maybe NodeId ->
  Int ->
  Int ->
  Bool ->
  RunConfig
mkRunConfig graph source target threads maxSteps trace =
  RunConfig
    { rcGraph = graph,
      rcSource = source,
      rcTarget = target,
      rcThreads = threads,
      rcMaxSteps = maxSteps,
      rcTrace = trace
    }

data VertexStepResult state msg = VertexStepResult
  { vsrState :: state,
    vsrOutgoing :: [(NodeId, msg)]
  }

type VertexStates state = Map NodeId state

type MessageQueues msg = Map NodeId [msg]
