module Output.Log
  ( DescribeLogEntry (..),
  )
where

import Algorithm.Log (LabelLogEntry (..), PathLogEntry (..), RankLogEntry (..))
import Algorithm.Messages (DistanceMsg (..), LabelMsg (..), RankMsg (..))
import Graph.Types (unNodeId)

class DescribeLogEntry log where
  describeLogEntry :: log -> String
  logEntrySortKey :: log -> (Int, Int, Int)

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
