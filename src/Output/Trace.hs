module Output.Trace
  ( describeResult,
    describeRun,
  )
where

import Data.List (sortBy)
import Pregel.Engine (PregelRun (..))
import Pregel.Types

describeRun :: Bool -> PregelRun -> String
describeRun verbose run =
  unlines
    ( map (describeSuperstep verbose) (prLogs run)
        ++ [""]
        ++ superstepSummary run
        ++ [describeResult (prResult run)]
    )

superstepSummary :: PregelRun -> [String]
superstepSummary run =
  [ "Converged in " ++ show (prSupersteps run) ++ " supersteps.",
    ""
  ]
    ++ if prMaxStepsReached run
      then
        [ "Warning: maximum superstep limit reached.",
          ""
        ]
      else []

describeSuperstep :: Bool -> SuperstepLog -> String
describeSuperstep verbose stepLog =
  unlines
    ( header
        : if verbose
          then map ("    " ++) (map describeLogEntry (sortedEntries stepLog))
          else []
    )
  where
    header =
      "Superstep "
        ++ show (sslStep stepLog)
        ++ ": "
        ++ show (sslActiveVertices stepLog)
        ++ " active vertices, "
        ++ show (sslMessagesSent stepLog)
        ++ " messages sent"

sortedEntries :: SuperstepLog -> [LogEntry]
sortedEntries stepLog =
  sortBy compareLogEntry (sslEntries stepLog)

compareLogEntry :: LogEntry -> LogEntry -> Ordering
compareLogEntry left right =
  case (left, right) of
    (VertexUpdated n1 _, VertexUpdated n2 _) ->
      compare n1 n2
    (VertexLabelUpdated n1 _, VertexLabelUpdated n2 _) ->
      compare n1 n2
    (VertexRankUpdated n1 _, VertexRankUpdated n2 _) ->
      compare n1 n2
    (MessageSent _ _ _, VertexUpdated _ _) ->
      GT
    (MessageSent _ _ _, VertexLabelUpdated _ _) ->
      GT
    (MessageSent _ _ _, VertexRankUpdated _ _) ->
      GT
    (VertexUpdated _ _, MessageSent _ _ _) ->
      LT
    (VertexLabelUpdated _ _, MessageSent _ _ _) ->
      LT
    (VertexRankUpdated _ _, MessageSent _ _ _) ->
      LT
    (MessageSent f1 t1 _, MessageSent f2 t2 _) ->
      compare f1 f2 <> compare t1 t2
    (VertexUpdated _ _, VertexLabelUpdated _ _) ->
      LT
    (VertexLabelUpdated _ _, VertexUpdated _ _) ->
      GT
    (VertexUpdated _ _, VertexRankUpdated _ _) ->
      LT
    (VertexRankUpdated _ _, VertexUpdated _ _) ->
      GT
    (VertexLabelUpdated _ _, VertexRankUpdated _ _) ->
      LT
    (VertexRankUpdated _ _, VertexLabelUpdated _ _) ->
      GT

describeLogEntry :: LogEntry -> String
describeLogEntry entry =
  case entry of
    VertexUpdated nodeId distance ->
      "vertex "
        ++ show nodeId
        ++ " updated: distance "
        ++ show distance
    VertexLabelUpdated nodeId label ->
      "vertex "
        ++ show nodeId
        ++ " updated: label "
        ++ show label
    VertexRankUpdated nodeId rank ->
      "vertex "
        ++ show nodeId
        ++ " updated: rank "
        ++ show rank
    MessageSent from to message ->
      "vertex "
        ++ show from
        ++ " -> "
        ++ show to
        ++ ": "
        ++ describeMessage message

describeMessage :: Message -> String
describeMessage (MsgDistance from distance) =
  "MsgDistance(from="
    ++ show from
    ++ ", dist="
    ++ show distance
    ++ ")"
describeMessage (MsgLabel label) =
  "MsgLabel(label=" ++ show label ++ ")"
describeMessage (MsgRank rank) =
  "MsgRank(rank=" ++ show rank ++ ")"

describeResult :: Result -> String
describeResult result =
  case result of
    PathFound path dist ->
      unlines
        [ "Result: path found",
          "  Distance: " ++ show dist,
          "  Path:     " ++ show path
        ]
    NoPath ->
      "Result: no path between source and target"
    ComponentFound label members ->
      unlines
        [ "Result: connected component",
          "  Label:  " ++ show label,
          "  Nodes:  " ++ show members
        ]
    Rankings pairs ->
      unlines
        ( "Result: PageRank"
            : map
              ( \(nodeId, rank) ->
                  "  node "
                    ++ show nodeId
                    ++ ": "
                    ++ show rank
              )
              pairs
        )
    NodeLabels pairs ->
      unlines
        ( "Result: label propagation"
            : map
              ( \(nodeId, label) ->
                  "  node "
                    ++ show nodeId
                    ++ " -> label "
                    ++ show label
              )
              pairs
        )
    InputError err ->
      "Result: invalid input — " ++ displayInputError err

displayInputError :: InputError -> String
displayInputError err =
  case err of
    MissingTarget ->
      "--target is required to compute a path"
    TargetNodeMissing nodeId ->
      "node " ++ show nodeId ++ " does not exist"
