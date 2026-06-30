module Graph.ValidationError
  ( GraphValidationError (..),
    RunNodeContext (..),
    displayGraphValidationError,
  )
where

import Graph.Types (NodeId)

data RunNodeContext
  = RunSource
  | RunTarget
  deriving (Eq, Show)

data GraphValidationError
  = RunNodeOutOfRange RunNodeContext NodeId Int
  deriving (Eq, Show)

displayGraphValidationError :: GraphValidationError -> String
displayGraphValidationError err =
  case err of
    RunNodeOutOfRange ctx nodeId maxNode ->
      showRunNodeContext ctx
        ++ " "
        ++ show nodeId
        ++ " out of range [0, "
        ++ show maxNode
        ++ "]"

showRunNodeContext :: RunNodeContext -> String
showRunNodeContext ctx =
  case ctx of
    RunSource -> "SOURCE"
    RunTarget -> "TARGET"
