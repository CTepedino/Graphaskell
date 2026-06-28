module Pregel.Types
  ( SuperstepLog (..),
    PregelRun (..),
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

data PathRunConfig = PathRunConfig
  { prcGraph :: Graph,
    prcSource :: NodeId,
    prcTarget :: NodeId,
    prcThreads :: Int,
    prcMaxSteps :: Int,
    prcTrace :: Bool
  }
  deriving (Eq, Show)

data RunConfig = RunConfig
  { rcGraph :: Graph,
    rcSource :: NodeId,
    rcThreads :: Int,
    rcMaxSteps :: Int,
    rcTrace :: Bool
  }
  deriving (Eq, Show)

pathRunConfigToRunConfig :: PathRunConfig -> RunConfig
pathRunConfigToRunConfig prc =
  RunConfig
    { rcGraph = prcGraph prc,
      rcSource = prcSource prc,
      rcThreads = prcThreads prc,
      rcMaxSteps = prcMaxSteps prc,
      rcTrace = prcTrace prc
    }

mkPathRunConfig ::
  Graph ->
  NodeId ->
  NodeId ->
  Int ->
  Int ->
  Bool ->
  PathRunConfig
mkPathRunConfig graph source target threads maxSteps trace =
  PathRunConfig
    { prcGraph = graph,
      prcSource = source,
      prcTarget = target,
      prcThreads = threads,
      prcMaxSteps = maxSteps,
      prcTrace = trace
    }

mkRunConfig :: Graph -> NodeId -> Int -> Int -> Bool -> RunConfig
mkRunConfig graph source threads maxSteps trace =
  RunConfig
    { rcGraph = graph,
      rcSource = source,
      rcThreads = threads,
      rcMaxSteps = maxSteps,
      rcTrace = trace
    }

data VertexStepResult state msg = VertexStepResult
  { vsrState :: state,
    vsrOutgoing :: [(NodeId, msg)]
  }
  deriving (Eq, Show)

type VertexStates state = Map NodeId state

type MessageQueues msg = Map NodeId [msg]
