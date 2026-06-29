module Algorithm.Name
  ( Algorithm (..),
  )
where

data Algorithm
  = BFS
  | BellmanFord
  | PageRank
  | ConnectedComponents
  | LabelPropagation
  deriving (Eq, Show, Read)
