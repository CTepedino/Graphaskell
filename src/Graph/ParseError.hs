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
  | DirSource
  deriving (Eq, Show)

data ParseContext
  = CtxNodes
  | CtxSource
  | CtxTarget
  | CtxEdgeFrom
  | CtxEdgeTo
  | CtxEdgeWeight
  | CtxAlgorithm
  deriving (Eq, Show)

data ParseError
  = MissingDirective Directive
  | NoEdges
  | UnknownLine String
  | InvalidPositiveInteger ParseContext String
  | InvalidNodeId ParseContext String
  | NodeOutOfRange ParseContext NodeId Int
  | UnknownAlgorithm String
  | WeightOnUnweightedGraph String
  | InvalidUnweightedEdge
  | InvalidWeightedEdge
  | WeightedModeMismatch
  deriving (Eq, Show)

displayParseError :: ParseError -> String
displayParseError err =
  case err of
    MissingDirective DirNodes ->
      "Falta la directiva NODES"
    MissingDirective DirSource ->
      "Falta la directiva SOURCE"
    NoEdges ->
      "Falta la seccion EDGES o no hay aristas definidas"
    UnknownLine line ->
      "Linea desconocida o fuera de la seccion EDGES: " ++ line
    InvalidPositiveInteger ctx raw ->
      showContext ctx ++ " debe ser un entero positivo: " ++ raw
    InvalidNodeId ctx raw ->
      showContext ctx ++ " debe ser un entero >= 0: " ++ raw
    NodeOutOfRange ctx nodeId maxNode ->
      showContext ctx
        ++ " "
        ++ show nodeId
        ++ " fuera de rango [0, "
        ++ show maxNode
        ++ "]"
    UnknownAlgorithm raw ->
      "Algoritmo desconocido: "
        ++ raw
        ++ " (use BFS, DFS o DIJKSTRA)"
    WeightOnUnweightedGraph line ->
      "Arista con peso encontrada pero falta la directiva WEIGHTED: " ++ line
    InvalidUnweightedEdge ->
      "Cada arista debe tener formato: <origen> <destino>"
    InvalidWeightedEdge ->
      "En modo WEIGHTED cada arista debe tener formato: <origen> <destino> <peso>"
    WeightedModeMismatch ->
      "Modo WEIGHTED requiere peso en todas las aristas"

showContext :: ParseContext -> String
showContext ctx =
  case ctx of
    CtxNodes -> "NODES"
    CtxSource -> "SOURCE"
    CtxTarget -> "TARGET"
    CtxEdgeFrom -> "arista (origen)"
    CtxEdgeTo -> "arista (destino)"
    CtxEdgeWeight -> "arista (peso)"
    CtxAlgorithm -> "ALGORITHM"
