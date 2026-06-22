module Algorithm.Types
  ( AlgorithmSpec (..),
  )
where

import Graph.Types
import Graph.VertexContext (VertexContext)
import Pregel.Types

data AlgorithmSpec = AlgorithmSpec
  { specInitState :: NodeId -> RunConfig -> VertexState,
    specBootstrap :: RunConfig -> [(NodeId, Message)],
    specVertexUpdate ::
      VertexContext ->
      VertexState ->
      [Message] ->
      VertexStepResult,
    specExtractResult :: VertexStates -> RunConfig -> Result
  }
