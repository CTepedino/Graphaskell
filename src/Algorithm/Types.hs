module Algorithm.Types
  ( AlgorithmSpec (..),
    mkLabelSpec,
    mkRankSpec,
    PathLog,
    LabelLog,
    RankLog,
    SomeAlgorithmSpec (..),
    someMaxSupersteps,
  )
where

import Algorithm.Common
  ( atLeastOneSuperstep,
    labelBootstrap,
    labelInitState,
    pageRankMaxSupersteps,
  )
import Algorithm.Error (AlgorithmError (..))
import Algorithm.Log (LabelLogEntry (..), MessageLog, PathLogEntry (..), RankLogEntry (..), DescribeLogEntry)
import Algorithm.Messages (DistanceMsg, LabelMsg, RankMsg)
import Algorithm.Observability (labelObserver, rankObserver)
import Algorithm.Result (Result)
import Algorithm.State (LabelState, RankState, emptyLabelState, emptyRankState)
import Data.Map.Strict (Map)
import Graph.Types
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

someMaxSupersteps :: SomeAlgorithmSpec -> Int -> Int
someMaxSupersteps (SomeAlgorithmSpec spec) =
  specMaxSupersteps spec

mkLabelSpec ::
  (VertexContext -> LabelState -> [LabelMsg] -> VertexStepResult LabelState LabelMsg) ->
  (Map NodeId LabelState -> RunConfig -> Either AlgorithmError Result) ->
  AlgorithmSpec LabelState LabelMsg LabelLog
mkLabelSpec vertexUpdate extractResult =
  AlgorithmSpec
    { specInitState = labelInitState,
      specDefaultState = emptyLabelState,
      specBootstrap = labelBootstrap,
      specVertexUpdate = vertexUpdate,
      specExtractResult = extractResult,
      specMaxSupersteps = atLeastOneSuperstep,
      specObserveStep = labelObserver
    }

mkRankSpec ::
  (NodeId -> RunConfig -> RankState) ->
  (RunConfig -> [(NodeId, RankMsg)]) ->
  (VertexContext -> RankState -> [RankMsg] -> VertexStepResult RankState RankMsg) ->
  (Map NodeId RankState -> RunConfig -> Either AlgorithmError Result) ->
  AlgorithmSpec RankState RankMsg RankLog
mkRankSpec initState bootstrap vertexUpdate extractResult =
  AlgorithmSpec
    { specInitState = initState,
      specDefaultState = emptyRankState,
      specBootstrap = bootstrap,
      specVertexUpdate = vertexUpdate,
      specExtractResult = extractResult,
      specMaxSupersteps = pageRankMaxSupersteps,
      specObserveStep = rankObserver
    }
