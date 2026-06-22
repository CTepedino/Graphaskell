module Pregel.Types
  ( Message (..),
    VertexState (..),
    LogEntry (..),
    SuperstepLog (..),
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
  | ComponentFound NodeId [NodeId]
  | Rankings [(NodeId, Double)]
  | NodeLabels [(NodeId, NodeId)]
  | InputError InputError
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
    rcAlgorithm :: Algorithm,
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
