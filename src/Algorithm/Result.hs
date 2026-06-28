module Algorithm.Result
  ( Result (..),
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
