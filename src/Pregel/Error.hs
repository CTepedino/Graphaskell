module Pregel.Error
  ( PregelError (..),
    displayPregelError,
  )
where

import Algorithm.Error (AlgorithmError, displayAlgorithmError)
import Graph.Types (NodeId)

data PregelError
  = MissingVertexContext NodeId
  | MissingMessageQueue NodeId
  | ResultExtraction AlgorithmError
  deriving (Eq, Show)

displayPregelError :: PregelError -> String
displayPregelError err =
  case err of
    MissingVertexContext nodeId ->
      "internal error: no vertex context for vertex "
        ++ show nodeId
    MissingMessageQueue nodeId ->
      "internal error: no message queue for vertex "
        ++ show nodeId
    ResultExtraction algoErr ->
      displayAlgorithmError algoErr
