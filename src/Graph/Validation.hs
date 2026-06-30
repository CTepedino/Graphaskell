module Graph.Validation
  ( validateRunNodes,
  )
where

import Data.Foldable (traverse_)
import Graph.Types (NodeId, ValidGraph, isValidNode, nodeCount)
import Graph.ValidationError
  ( GraphValidationError (..),
    RunNodeContext (..),
  )

validateRunNodes ::
  ValidGraph -> Maybe NodeId -> Maybe NodeId -> Either GraphValidationError ()
validateRunNodes graph mSource mTarget = do
  traverse_ (validateNodeInGraph graph RunSource) mSource
  traverse_ (validateNodeInGraph graph RunTarget) mTarget

validateNodeInGraph ::
  ValidGraph -> RunNodeContext -> NodeId -> Either GraphValidationError ()
validateNodeInGraph graph ctx nodeId
  | isValidNode graph nodeId = Right ()
  | otherwise =
      Left (RunNodeOutOfRange ctx nodeId (nodeCount graph - 1))
