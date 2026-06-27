module Algorithm.Result
  ( Result (..),
    describeResult,
  )
where

import Graph.Types (NodeId)

data Result
  = PathFound [NodeId] Int
  | NoPath
  | Components [(NodeId, [NodeId])]
  | Rankings [(NodeId, Double)]
  | NodeLabels [(NodeId, NodeId)]
  deriving (Eq, Show)

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
