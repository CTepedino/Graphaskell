module Algorithm.Types
  ( AlgorithmSpec (..),
    PathLog,
    LabelLog,
    RankLog,
    SomeAlgorithmSpec (..),
  )
where

import Algorithm.Error (AlgorithmError (..))
import Algorithm.Log (LabelLogEntry (..), MessageLog, PathLogEntry (..), RankLogEntry (..), DescribeLogEntry)
import Algorithm.Messages (DistanceMsg, LabelMsg, RankMsg)
import Algorithm.Result (Result)
import Data.Map.Strict (Map)
import Graph.Types (NodeId)
import Graph.VertexContext (VertexContext)
import Pregel.Types (RunConfig, VertexStepResult)

data AlgorithmSpec state msg log = AlgorithmSpec
  { specInitState :: NodeId -> RunConfig -> state,
    specDefaultState :: state,
    specBootstrap :: RunConfig -> [(NodeId, msg)],
    specVertexUpdate ::
      VertexContext ->
      state ->
      [msg] ->
      VertexStepResult state msg,
    specExtractResult :: Map NodeId state -> RunConfig -> Either AlgorithmError Result,
    specMaxSupersteps :: Int -> Int,
    specObserveStep ::
      NodeId ->
      state ->
      state ->
      [(NodeId, msg)] ->
      [log]
  }

type PathLog = PathLogEntry DistanceMsg

type LabelLog = LabelLogEntry LabelMsg

type RankLog = RankLogEntry RankMsg

data SomeAlgorithmSpec where
  SomeAlgorithmSpec ::
    (DescribeLogEntry log, MessageLog msg log, Eq log, Show log, Eq state) =>
    AlgorithmSpec state msg log ->
    SomeAlgorithmSpec
