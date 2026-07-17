module Algorithm.State
  ( PathState (..),
    LabelState (..),
    RankState (..),
    emptyPathState,
    emptyLabelState,
    emptyRankState,
  )
where

import Graph.Types (Distance, NodeId (..))

data PathState = PathState
  { psDistance :: Maybe Distance,
    psPredecessor :: Maybe NodeId
  }
  deriving (Eq, Show)

newtype LabelState = LabelState
  { lsLabel :: NodeId
  }
  deriving (Eq, Show)

newtype RankState = RankState
  { rsRank :: Double
  }
  deriving Eq

emptyPathState :: PathState
emptyPathState =
  PathState
    { psDistance = Nothing,
      psPredecessor = Nothing
    }

emptyLabelState :: LabelState
emptyLabelState =
  LabelState (NodeId 0)

emptyRankState :: RankState
emptyRankState =
  RankState 0
