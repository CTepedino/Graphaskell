module Algorithm.State
  ( PathState (..),
    LabelState (..),
    RankState (..),
    emptyPathState,
    emptyLabelState,
    emptyRankState,
  )
where

import Graph.Types (NodeId)

data PathState = PathState
  { psDistance :: Maybe Int,
    psPredecessor :: Maybe NodeId
  }
  deriving (Eq, Show)

data LabelState = LabelState
  { lsLabel :: NodeId
  }
  deriving (Eq, Show)

data RankState = RankState
  { rsRank :: Double
  }
  deriving (Eq, Show)

emptyPathState :: PathState
emptyPathState =
  PathState
    { psDistance = Nothing,
      psPredecessor = Nothing
    }

emptyLabelState :: LabelState
emptyLabelState =
  LabelState 0

emptyRankState :: RankState
emptyRankState =
  RankState 0
