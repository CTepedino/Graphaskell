module Algorithm.Messages
  ( DistanceMsg (..),
    LabelMsg (..),
    RankMsg (..),
  )
where

import Graph.Types (NodeId)

data DistanceMsg = DistanceMsg
  { dmFrom :: NodeId,
    dmDistance :: Int
  }
  deriving (Eq, Show)

data LabelMsg = LabelMsg
  { lmLabel :: NodeId
  }
  deriving (Eq, Show)

data RankMsg = RankMsg
  { rmRank :: Double
  }
  deriving (Eq, Show)
