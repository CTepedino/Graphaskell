module Graph.Validation
  ( validateRunNodes,
  )
where

import Graph.Types (NodeId, ValidGraph, isValidNode, nodeCount)
import Graph.ValidationError
  ( GraphValidationError (..),
    RunNodeContext (..),
  )

validateRunNodes :: ValidGraph -> Maybe NodeId -> Maybe NodeId -> Either GraphValidationError ()
validateRunNodes graph mSource mTarget = do
  maybe (Right ()) (validateNodeInGraph graph RunSource) mSource
  maybe (Right ()) (validateNodeInGraph graph RunTarget) mTarget

validateNodeInGraph :: ValidGraph -> RunNodeContext -> NodeId -> Either GraphValidationError ()
validateNodeInGraph graph ctx nodeId
  | isValidNode graph nodeId =
      Right ()
  | otherwise =
      Left (RunNodeOutOfRange ctx nodeId (nodeCount graph - 1))
