module Pregel.Error
  ( PregelError (..),
    displayPregelError,
  )
where

import Graph.Types (NodeId)

data PregelError
  = MissingVertexContext NodeId
  | MissingMessageQueue NodeId
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
