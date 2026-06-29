module Algorithm.Log
  ( PathLogEntry (..),
    LabelLogEntry (..),
    RankLogEntry (..),
    MessageLog (..),
    messageSentLogs,
    DescribeLogEntry (..),
  )
where

import Algorithm.Messages (DistanceMsg (..), LabelMsg (..), RankMsg (..))
import Graph.Types (Distance, NodeId, unNodeId)

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

class DescribeLogEntry log where
  describeLogEntry :: log -> String
  logEntrySortKey :: log -> (Int, Int, Int)

instance MessageLog DistanceMsg (PathLogEntry DistanceMsg) where
  messageSentLog = PathMessageSent

instance MessageLog LabelMsg (LabelLogEntry LabelMsg) where
  messageSentLog = LabelMessageSent

instance MessageLog RankMsg (RankLogEntry RankMsg) where
  messageSentLog = RankMessageSent

instance DescribeLogEntry (PathLogEntry DistanceMsg) where
  describeLogEntry entry =
    case entry of
      PathDistanceUpdated nodeId distance ->
        "vertex "
          ++ show nodeId
          ++ " updated: distance "
          ++ show distance
      PathMessageSent from to message ->
        "vertex "
          ++ show from
          ++ " -> "
          ++ show to
          ++ ": "
          ++ show message
  logEntrySortKey entry =
    case entry of
      PathDistanceUpdated nodeId _ ->
        (0, unNodeId nodeId, 0)
      PathMessageSent from to _ ->
        (1, unNodeId from, unNodeId to)

instance DescribeLogEntry (LabelLogEntry LabelMsg) where
  describeLogEntry entry =
    case entry of
      LabelChanged nodeId label ->
        "vertex "
          ++ show nodeId
          ++ " updated: label "
          ++ show label
      LabelMessageSent from to message ->
        "vertex "
          ++ show from
          ++ " -> "
          ++ show to
          ++ ": "
          ++ show message
  logEntrySortKey entry =
    case entry of
      LabelChanged nodeId _ ->
        (0, unNodeId nodeId, 0)
      LabelMessageSent from to _ ->
        (1, unNodeId from, unNodeId to)

instance DescribeLogEntry (RankLogEntry RankMsg) where
  describeLogEntry entry =
    case entry of
      RankUpdated nodeId rank ->
        "vertex "
          ++ show nodeId
          ++ " updated: rank "
          ++ show rank
      RankMessageSent from to message ->
        "vertex "
          ++ show from
          ++ " -> "
          ++ show to
          ++ ": "
          ++ show message
  logEntrySortKey entry =
    case entry of
      RankUpdated nodeId _ ->
        (0, unNodeId nodeId, 0)
      RankMessageSent from to _ ->
        (1, unNodeId from, unNodeId to)
