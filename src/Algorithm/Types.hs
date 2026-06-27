module Algorithm.Types
  ( AlgorithmSpec (..),
    SomeAlgorithmSpec (..),
  )
where

import Algorithm.Error (AlgorithmError (..))
import Algorithm.Log (DescribeLogEntry)
import Algorithm.Result (Result)
import Data.Map.Strict (Map)
import Graph.Types
import Graph.VertexContext (VertexContext)
import Pregel.Types

data AlgorithmSpec state msg log = AlgorithmSpec
  { specInitState :: NodeId -> RunConfig -> state,
    specDefaultState :: state,
    specBootstrap :: RunConfig -> [(NodeId, msg)],
    specVertexUpdate ::
      VertexContext ->
      state ->
      [msg] ->
      VertexStepResult state msg log,
    specExtractResult :: Map NodeId state -> RunConfig -> Either AlgorithmError Result,
    specMaxSupersteps :: Int -> Int
  }

data SomeAlgorithmSpec where
  SomeAlgorithmSpec :: DescribeLogEntry log => AlgorithmSpec state msg log -> SomeAlgorithmSpec
