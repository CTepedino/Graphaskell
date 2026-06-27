module Algorithm.Log
  ( PathLogEntry (..),
    LabelLogEntry (..),
    RankLogEntry (..),
    DescribeLogEntry (..),
    MessageLog (..),
  )
where

import Algorithm.Messages (DistanceMsg (..), LabelMsg (..), RankMsg (..))
import Graph.Types (NodeId)

class DescribeLogEntry log where
  describeLogEntry :: log -> String
  logEntrySortKey :: log -> (Int, Int, Int)

class MessageLog msg log where
  messageSentLog :: NodeId -> NodeId -> msg -> log

data PathLogEntry msg
  = PathDistanceUpdated NodeId Int
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
        (0, nodeId, 0)
      PathMessageSent from to _ ->
        (1, from, to)

instance MessageLog DistanceMsg (PathLogEntry DistanceMsg) where
  messageSentLog = PathMessageSent

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
        (0, nodeId, 0)
      LabelMessageSent from to _ ->
        (1, from, to)

instance MessageLog LabelMsg (LabelLogEntry LabelMsg) where
  messageSentLog = LabelMessageSent

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
        (0, nodeId, 0)
      RankMessageSent from to _ ->
        (1, from, to)

instance MessageLog RankMsg (RankLogEntry RankMsg) where
  messageSentLog = RankMessageSent
