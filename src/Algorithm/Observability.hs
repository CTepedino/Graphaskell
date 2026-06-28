module Algorithm.Observability
  ( pathObserver,
    labelObserver,
    rankObserver,
  )
where

import Algorithm.Log
  ( LabelLogEntry (..),
    PathLogEntry (..),
    RankLogEntry (..),
  )
import Algorithm.Messages (DistanceMsg, LabelMsg, RankMsg)
import Algorithm.State (LabelState (..), PathState (..), RankState (..))
import Graph.Types (NodeId)

pathObserver ::
  NodeId ->
  PathState ->
  PathState ->
  [(NodeId, DistanceMsg)] ->
  [PathLogEntry DistanceMsg]
pathObserver nodeId old new _ =
  case psDistance new of
    Just dist | psDistance old /= psDistance new ->
      [PathDistanceUpdated nodeId dist]
    _ ->
      []

labelObserver ::
  NodeId ->
  LabelState ->
  LabelState ->
  [(NodeId, LabelMsg)] ->
  [LabelLogEntry LabelMsg]
labelObserver nodeId old new _ =
  if lsLabel old == lsLabel new
    then []
    else [LabelChanged nodeId (lsLabel new)]

rankObserver ::
  NodeId ->
  RankState ->
  RankState ->
  [(NodeId, RankMsg)] ->
  [RankLogEntry RankMsg]
rankObserver nodeId old new _ =
  if rsRank old == rsRank new
    then []
    else [RankUpdated nodeId (rsRank new)]
