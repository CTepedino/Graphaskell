module Algorithm.Types
  ( AlgorithmSpec (..),
    SomeAlgorithmSpec (..),
  )
where

import Algorithm.Error (AlgorithmError (..))
import Data.Map.Strict (Map)
import Graph.Types
import Graph.VertexContext (VertexContext)
import Pregel.Types

data AlgorithmSpec state msg = AlgorithmSpec
  { specInitState :: NodeId -> RunConfig -> state,
    specDefaultState :: state,
    specBootstrap :: RunConfig -> [(NodeId, msg)],
    specVertexUpdate ::
      VertexContext ->
      state ->
      [msg] ->
      VertexStepResult state msg,
    specExtractResult :: Map NodeId state -> RunConfig -> Either AlgorithmError Result,
    specMaxSupersteps :: Int -> Int
  }

data SomeAlgorithmSpec where
  SomeAlgorithmSpec :: Show msg => AlgorithmSpec state msg -> SomeAlgorithmSpec
