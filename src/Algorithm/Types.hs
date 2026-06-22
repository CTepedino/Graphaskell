module Algorithm.Types
  ( AlgorithmSpec (..),
  )
where

import Graph.Types
import Pregel.Types

data AlgorithmSpec = AlgorithmSpec
  { specInitState :: NodeId -> RunConfig -> VertexState,
    specBootstrap :: RunConfig -> [(NodeId, Message)],
    specVertexUpdate ::
      Graph ->
      VertexStates ->
      NodeId ->
      VertexState ->
      [Message] ->
      VertexStepResult,
    specExtractResult :: VertexStates -> RunConfig -> Result
  }
