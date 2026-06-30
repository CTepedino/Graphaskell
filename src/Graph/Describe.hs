module Graph.Describe
  ( describeGraph,
  )
where

import Graph.Types
  ( ValidGraph,
    defaultEdgeWeight,
    graphEdges,
    graphNodes,
    neighbors,
    nodeCount,
  )

describeGraph :: ValidGraph -> String
describeGraph graph =
  unlines
    [ "  Nodes:      " ++ show (nodeCount graph),
      "  Edges:      " ++ show (length (graphEdges graph)),
      "",
      adjSummary graph
    ]

adjSummary :: ValidGraph -> String
adjSummary graph =
  unlines
    ( "  Adjacency:"
        : map formatAdj (graphNodes graph)
    )
  where
    formatAdj nodeId =
      let nbs = neighbors graph nodeId
       in "    "
            ++ show nodeId
            ++ " -> "
            ++ if null nbs
              then "[]"
              else unwords (map formatNeighbor nbs)
    formatNeighbor (to, weight)
      | weight == defaultEdgeWeight = show to
      | otherwise = show to ++ "(" ++ show weight ++ ")"
