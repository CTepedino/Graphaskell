module Algorithm.Types
  ( AlgorithmSpec (..),
    GlobalAlgorithmSpec (..),
    mkLabelGlobalSpec,
    mkRankGlobalSpec,
    PathAlgorithmSpec (..),
    PathLog,
    LabelLog,
    RankLog,
    SomeAlgorithmSpec (..),
    globalRunSpec,
    pathRunSpec,
  )
where

import Algorithm.Common
  ( atLeastOneSuperstep,
    labelBootstrap,
    labelInitState,
    pageRankMaxSupersteps,
  )
import Algorithm.Error (AlgorithmError (..))
import Algorithm.Log (LabelLogEntry (..), MessageLog, PathLogEntry (..), RankLogEntry (..))
import Algorithm.Messages (DistanceMsg, LabelMsg, RankMsg)
import Output.Log (DescribeLogEntry)
import Algorithm.Observability (labelObserver, pathObserver, rankObserver)
import Algorithm.Result (Result)
import Algorithm.State (LabelState, PathState, RankState, emptyLabelState, emptyRankState)
import Data.Map.Strict (Map)
import Graph.Types
import Graph.VertexContext (VertexContext)
import Pregel.Types (PathRunConfig, RunConfig, VertexStepResult)

data AlgorithmSpec state msg log = AlgorithmSpec
  { specInitState :: NodeId -> RunConfig -> state,
    specDefaultState :: state,
    specBootstrap :: RunConfig -> [(NodeId, msg)],
    specVertexUpdate ::
      VertexContext ->
      state ->
      [msg] ->
      VertexStepResult state msg,
    specExtractResult :: Map NodeId state -> Either AlgorithmError Result,
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

data PathAlgorithmSpec = PathAlgorithmSpec
  { psInitState :: NodeId -> PathRunConfig -> PathState,
    psDefaultState :: PathState,
    psBootstrap :: PathRunConfig -> [(NodeId, DistanceMsg)],
    psVertexUpdate ::
      VertexContext ->
      PathState ->
      [DistanceMsg] ->
      VertexStepResult PathState DistanceMsg,
    psExtractResult ::
      Map NodeId PathState ->
      PathRunConfig ->
      Either AlgorithmError Result,
    psMaxSupersteps :: Int -> Int
  }

data SomeAlgorithmSpec where
  SomePathAlgorithmSpec :: PathAlgorithmSpec -> SomeAlgorithmSpec
  SomeGlobalAlgorithmSpec ::
    (DescribeLogEntry log, MessageLog msg log, Eq log, Show log) =>
    GlobalAlgorithmSpec state msg log ->
    SomeAlgorithmSpec

pathRunSpec ::
  PathAlgorithmSpec ->
  PathRunConfig ->
  AlgorithmSpec PathState DistanceMsg PathLog
pathRunSpec ps prc =
  AlgorithmSpec
    { specInitState = \nodeId _cfg -> psInitState ps nodeId prc,
      specDefaultState = psDefaultState ps,
      specBootstrap = \_cfg -> psBootstrap ps prc,
      specVertexUpdate = psVertexUpdate ps,
      specExtractResult = \states -> psExtractResult ps states prc,
      specMaxSupersteps = psMaxSupersteps ps,
      specObserveStep = pathObserver
    }

data GlobalAlgorithmSpec state msg log = GlobalAlgorithmSpec
  { globalInitState :: NodeId -> RunConfig -> state,
    globalDefaultState :: state,
    globalBootstrap :: RunConfig -> [(NodeId, msg)],
    globalVertexUpdate ::
      VertexContext ->
      state ->
      [msg] ->
      VertexStepResult state msg,
    globalExtractResult :: Map NodeId state -> RunConfig -> Either AlgorithmError Result,
    globalMaxSupersteps :: Int -> Int,
    globalObserveStep ::
      NodeId ->
      state ->
      state ->
      [(NodeId, msg)] ->
      [log]
  }

mkLabelGlobalSpec ::
  (VertexContext -> LabelState -> [LabelMsg] -> VertexStepResult LabelState LabelMsg) ->
  (Map NodeId LabelState -> RunConfig -> Either AlgorithmError Result) ->
  GlobalAlgorithmSpec LabelState LabelMsg LabelLog
mkLabelGlobalSpec vertexUpdate extractResult =
  GlobalAlgorithmSpec
    { globalInitState = labelInitState,
      globalDefaultState = emptyLabelState,
      globalBootstrap = labelBootstrap,
      globalVertexUpdate = vertexUpdate,
      globalExtractResult = extractResult,
      globalMaxSupersteps = atLeastOneSuperstep,
      globalObserveStep = labelObserver
    }

mkRankGlobalSpec ::
  (NodeId -> RunConfig -> RankState) ->
  (RunConfig -> [(NodeId, RankMsg)]) ->
  (VertexContext -> RankState -> [RankMsg] -> VertexStepResult RankState RankMsg) ->
  (Map NodeId RankState -> RunConfig -> Either AlgorithmError Result) ->
  GlobalAlgorithmSpec RankState RankMsg RankLog
mkRankGlobalSpec initState bootstrap vertexUpdate extractResult =
  GlobalAlgorithmSpec
    { globalInitState = initState,
      globalDefaultState = emptyRankState,
      globalBootstrap = bootstrap,
      globalVertexUpdate = vertexUpdate,
      globalExtractResult = extractResult,
      globalMaxSupersteps = pageRankMaxSupersteps,
      globalObserveStep = rankObserver
    }

globalRunSpec ::
  GlobalAlgorithmSpec state msg log ->
  RunConfig ->
  AlgorithmSpec state msg log
globalRunSpec gs cfg =
  AlgorithmSpec
    { specInitState = globalInitState gs,
      specDefaultState = globalDefaultState gs,
      specBootstrap = globalBootstrap gs,
      specVertexUpdate = globalVertexUpdate gs,
      specExtractResult = \states -> globalExtractResult gs states cfg,
      specMaxSupersteps = globalMaxSupersteps gs,
      specObserveStep = globalObserveStep gs
    }
