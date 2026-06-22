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
  | MsgVisit NodeId
  deriving (Eq, Show)

data VertexState = VertexState
  { vsDistance :: Maybe Int,
    vsPredecessor :: Maybe NodeId,
    vsVisited :: Bool,
    vsPath :: Maybe [NodeId]
  }
  deriving (Eq, Show)

initialVertexState :: VertexState
initialVertexState =
  VertexState
    { vsDistance = Nothing,
      vsPredecessor = Nothing,
      vsVisited = False,
      vsPath = Nothing
    }

data LogEntry
  = VertexUpdated NodeId Int
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
    vsrLogs :: [LogEntry],
    vsrChanged :: Bool
  }
  deriving (Eq, Show)

type VertexStates = Map NodeId VertexState

type MessageQueues = Map NodeId [Message]
