module Algorithm.Result
  ( Result (..),
  )
where

import Graph.Types (Distance, NodeId)

data Result
  = PathFound [NodeId] Distance
  | NoPath
  | Components [(NodeId, [NodeId])]
  | Rankings [(NodeId, Double)]
  | NodeLabels [(NodeId, NodeId)]
  deriving (Eq, Show)
