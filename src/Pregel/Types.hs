module Pregel.Types
  ( DistanceMsg (..),
    LabelMsg (..),
    RankMsg (..),
    LogEntry (..),
    logEntrySortKey,
    SuperstepLog (..),
    PregelRun (..),
    SomePregelRun (..),
    somePregelResult,
    Result (..),
    RunConfig (..),
    VertexStepResult (..),
    VertexStates,
    MessageQueues,
  )
where

import Data.Map.Strict (Map)
import Graph.Types

data DistanceMsg = DistanceMsg
  { dmFrom :: NodeId,
    dmDistance :: Int
  }
  deriving (Eq, Show)

data LabelMsg = LabelMsg
  { lmLabel :: NodeId
  }
  deriving (Eq, Show)

data RankMsg = RankMsg
  { rmRank :: Double
  }
  deriving (Eq, Show)

data LogEntry msg
  = VertexUpdated NodeId Int
  | VertexLabelUpdated NodeId NodeId
  | VertexRankUpdated NodeId Double
  | MessageSent NodeId NodeId msg
  deriving (Eq, Show)

logEntrySortKey :: LogEntry msg -> (Int, Int, Int)
logEntrySortKey entry =
  case entry of
    VertexUpdated nodeId _ ->
      (0, nodeId, 0)
    VertexLabelUpdated nodeId _ ->
      (1, nodeId, 0)
    VertexRankUpdated nodeId _ ->
      (2, nodeId, 0)
    MessageSent from to _ ->
      (3, from, to)

data SuperstepLog msg = SuperstepLog
  { sslStep :: Int,
    sslActiveVertices :: Int,
    sslMessagesSent :: Int,
    sslEntries :: [LogEntry msg]
  }
  deriving (Eq, Show)

data Result
  = PathFound [NodeId] Int
  | NoPath
  | Components [(NodeId, [NodeId])]
  | Rankings [(NodeId, Double)]
  | NodeLabels [(NodeId, NodeId)]
  deriving (Eq, Show)

data PregelRun msg = PregelRun
  { prSupersteps :: Int,
    prLogs :: [SuperstepLog msg],
    prResult :: Result,
    prMaxStepsReached :: Bool
  }
  deriving (Eq, Show)

data SomePregelRun where
  SomePregelRun :: Show msg => PregelRun msg -> SomePregelRun

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

data VertexStepResult state msg = VertexStepResult
  { vsrState :: state,
    vsrOutgoing :: [(NodeId, msg)],
    vsrLogs :: [LogEntry msg]
  }
  deriving (Eq, Show)

type VertexStates state = Map NodeId state

type MessageQueues msg = Map NodeId [msg]
