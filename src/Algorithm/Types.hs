module Algorithm.Types
  ( AlgorithmSpec (..),
    GlobalAlgorithmSpec (..),
    PathAlgorithmSpec (..),
    PathLog,
    SomeAlgorithmSpec (..),
    globalRunSpec,
    pathRunSpec,
  )
where

import Algorithm.Error (AlgorithmError (..))
import Algorithm.Log (DescribeLogEntry, PathLogEntry (..))
import Algorithm.Messages (DistanceMsg)
import Algorithm.Observability (pathObserver)
import Algorithm.Result (Result)
import Algorithm.State (PathState)
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
      VertexStepResult state msg log,
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

data PathAlgorithmSpec = PathAlgorithmSpec
  { psInitState :: NodeId -> PathRunConfig -> PathState,
    psDefaultState :: PathState,
    psBootstrap :: PathRunConfig -> [(NodeId, DistanceMsg)],
    psVertexUpdate ::
      VertexContext ->
      PathState ->
      [DistanceMsg] ->
      VertexStepResult PathState DistanceMsg PathLog,
    psExtractResult ::
      Map NodeId PathState ->
      PathRunConfig ->
      Either AlgorithmError Result,
    psMaxSupersteps :: Int -> Int
  }

data SomeAlgorithmSpec where
  SomePathAlgorithmSpec :: PathAlgorithmSpec -> SomeAlgorithmSpec
  SomeGlobalAlgorithmSpec ::
    (DescribeLogEntry log, Eq log, Show log) =>
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
      VertexStepResult state msg log,
    globalExtractResult :: Map NodeId state -> RunConfig -> Either AlgorithmError Result,
    globalMaxSupersteps :: Int -> Int,
    globalObserveStep ::
      NodeId ->
      state ->
      state ->
      [(NodeId, msg)] ->
      [log]
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
