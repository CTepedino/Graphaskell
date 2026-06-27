module Pregel.Types
  ( Message (..),
    VertexState (..),
    LogEntry (..),
    logEntrySortKey,
    SuperstepLog (..),
    PregelRun (..),
    Result (..),
    InputError (..),
    RunConfig (..),
    VertexStepResult (..),
    VertexStates,
    MessageQueues,
    initialVertexState,
  )
where

import Data.Map.Strict (Map)
import Graph.Types

data Message
  = MsgDistance NodeId Int
  | MsgLabel NodeId
  | MsgRank Double
  deriving (Eq, Show)

data VertexState = VertexState
  { vsDistance :: Maybe Int,
    vsPredecessor :: Maybe NodeId,
    vsLabel :: Maybe NodeId,
    vsRank :: Maybe Double
  }
  deriving (Eq, Show)

initialVertexState :: VertexState
initialVertexState =
  VertexState
    { vsDistance = Nothing,
      vsPredecessor = Nothing,
      vsLabel = Nothing,
      vsRank = Nothing
    }

data LogEntry
  = VertexUpdated NodeId Int
  | VertexLabelUpdated NodeId NodeId
  | VertexRankUpdated NodeId Double
  | MessageSent NodeId NodeId Message
  deriving (Eq, Show)

logEntrySortKey :: LogEntry -> (Int, Int, Int)
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

data SuperstepLog = SuperstepLog
  { sslStep :: Int,
    sslActiveVertices :: Int,
    sslMessagesSent :: Int,
    sslEntries :: [LogEntry]
  }
  deriving (Eq, Show)

data Result
  = PathFound [NodeId] Int
  | NoPath
  | Components [(NodeId, [NodeId])]
  | Rankings [(NodeId, Double)]
  | NodeLabels [(NodeId, NodeId)]
  | InputError InputError
  deriving (Eq, Show)

data PregelRun = PregelRun
  { prSupersteps :: Int,
    prLogs :: [SuperstepLog],
    prResult :: Result,
    prMaxStepsReached :: Bool
  }
  deriving (Eq, Show)

data InputError
  = MissingTarget
  | TargetNodeMissing NodeId
  deriving (Eq, Show)

data RunConfig = RunConfig
  { rcGraph :: Graph,
    rcSource :: NodeId,
    rcTarget :: Maybe NodeId,
    rcThreads :: Int,
    rcMaxSteps :: Int
  }
  deriving (Eq, Show)

data VertexStepResult = VertexStepResult
  { vsrState :: VertexState,
    vsrOutgoing :: [(NodeId, Message)],
    vsrLogs :: [LogEntry]
  }
  deriving (Eq, Show)

type VertexStates = Map NodeId VertexState

type MessageQueues = Map NodeId [Message]
