module Graph.ParseError
  ( ParseError (..),
    Directive (..),
    ParseContext (..),
    displayParseError,
  )
where

import Graph.Types (NodeId)

data Directive
  = DirNodes
  deriving (Eq, Show)

data ParseContext
  = CtxNodes
  | CtxSource
  | CtxTarget
  | CtxEdgeFrom
  | CtxEdgeTo
  | CtxEdgeWeight
  deriving (Eq, Show)

data ParseError
  = MissingDirective Directive
  | NoEdges
  | UnknownLine String
  | InvalidPositiveInteger ParseContext String
  | InvalidNodeId ParseContext String
  | NodeOutOfRange ParseContext NodeId Int
  | WeightOnUnweightedGraph String
  | InvalidUnweightedEdge
  | InvalidWeightedEdge
  | WeightedModeMismatch
  deriving (Eq, Show)

displayParseError :: ParseError -> String
displayParseError err =
  case err of
    MissingDirective DirNodes ->
      "Missing NODES directive"
    NoEdges ->
      "Missing EDGES section or no edges defined"
    UnknownLine line ->
      "Unknown line or outside EDGES section: " ++ line
    InvalidPositiveInteger ctx raw ->
      showContext ctx ++ " must be a positive integer: " ++ raw
    InvalidNodeId ctx raw ->
      showContext ctx ++ " must be an integer >= 0: " ++ raw
    NodeOutOfRange ctx nodeId maxNode ->
      showContext ctx
        ++ " "
        ++ show nodeId
        ++ " out of range [0, "
        ++ show maxNode
        ++ "]"
    WeightOnUnweightedGraph line ->
      "Weighted edge found but WEIGHTED directive is missing: " ++ line
    InvalidUnweightedEdge ->
      "Each edge must have the format: <from> <to>"
    InvalidWeightedEdge ->
      "In WEIGHTED mode each edge must have the format: <from> <to> <weight>"
    WeightedModeMismatch ->
      "WEIGHTED mode requires a weight on every edge"

showContext :: ParseContext -> String
showContext ctx =
  case ctx of
    CtxNodes -> "NODES"
    CtxSource -> "SOURCE"
    CtxTarget -> "TARGET"
    CtxEdgeFrom -> "edge (from)"
    CtxEdgeTo -> "edge (to)"
    CtxEdgeWeight -> "edge (weight)"
