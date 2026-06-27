module Output.Trace
  ( describeRun,
  )
where

import Algorithm.Log (DescribeLogEntry (..))
import Algorithm.Result (describeResult)
import Data.List (sortBy)
import Data.Ord (comparing)
import Pregel.Types

describeRun :: DescribeLogEntry log => Bool -> PregelRun log -> String
describeRun verbose run =
  unlines
    ( map (describeSuperstep verbose) (prLogs run)
        ++ [""]
        ++ superstepSummary run
        ++ [describeResult (prResult run)]
    )

superstepSummary :: PregelRun log -> [String]
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

describeSuperstep :: DescribeLogEntry log => Bool -> SuperstepLog log -> String
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

sortedEntries :: DescribeLogEntry log => SuperstepLog log -> [log]
sortedEntries stepLog =
  sortBy (comparing logEntrySortKey) (sslEntries stepLog)
