module Algorithm.Log
  ( PathLogEntry (..),
    LabelLogEntry (..),
    RankLogEntry (..),
    MessageLog (..),
    messageSentLogs,
  )
where

import Algorithm.Messages (DistanceMsg (..), LabelMsg (..), RankMsg (..))
import Graph.Types (Distance, NodeId)

class MessageLog msg log where
  messageSentLog :: NodeId -> NodeId -> msg -> log

messageSentLogs ::
  MessageLog msg log =>
  NodeId ->
  [(NodeId, msg)] ->
  [log]
messageSentLogs from outgoing =
  [ messageSentLog from to msg
    | (to, msg) <- outgoing
  ]

data PathLogEntry msg
  = PathDistanceUpdated NodeId Distance
  | PathMessageSent NodeId NodeId msg
  deriving (Eq, Show)

data LabelLogEntry msg
  = LabelChanged NodeId NodeId
  | LabelMessageSent NodeId NodeId msg
  deriving (Eq, Show)

data RankLogEntry msg
  = RankUpdated NodeId Double
  | RankMessageSent NodeId NodeId msg
  deriving (Eq, Show)

instance MessageLog DistanceMsg (PathLogEntry DistanceMsg) where
  messageSentLog = PathMessageSent

instance MessageLog LabelMsg (LabelLogEntry LabelMsg) where
  messageSentLog = LabelMessageSent

instance MessageLog RankMsg (RankLogEntry RankMsg) where
  messageSentLog = RankMessageSent
