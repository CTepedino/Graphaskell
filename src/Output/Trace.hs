module Output.Trace
  ( describeRun,
  )
where

import Data.List (sortBy)
import Data.Ord (comparing)
import Pregel.Types

describeRun :: Show msg => Bool -> PregelRun msg -> String
describeRun verbose run =
  unlines
    ( map (describeSuperstep verbose) (prLogs run)
        ++ [""]
        ++ superstepSummary run
        ++ [describeResult (prResult run)]
    )

superstepSummary :: PregelRun msg -> [String]
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

describeSuperstep :: Show msg => Bool -> SuperstepLog msg -> String
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

sortedEntries :: SuperstepLog msg -> [LogEntry msg]
sortedEntries stepLog =
  sortBy (comparing logEntrySortKey) (sslEntries stepLog)

describeLogEntry :: Show msg => LogEntry msg -> String
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

describeMessage :: Show msg => msg -> String
describeMessage = show

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
    Components groups ->
      unlines
        ( "Result: connected components"
            : map
              ( \(label, members) ->
                  "  component "
                    ++ show label
                    ++ ": "
                    ++ show members
              )
              groups
        )
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
