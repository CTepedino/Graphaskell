module Graph.Parser
  ( LoadGraphError (..),
    loadGraphFile,
    parseGraphFile,
    describeGraph,
    validateRunNodes,
    validateRunNodesForAlgorithm,
    displayLoadGraphError,
  )
where

import Control.Exception (IOException, try)
import Control.Monad ((<=<), foldM)
import Data.Foldable (traverse_)
import Algorithm.Name (Algorithm (..))
import Graph.ParseError
import Util.Reading (readNonNegativeInt, readPositiveInt, trim)
import Graph.Types
  ( Edge (..),
    GraphEndpoint (..),
    GraphError (..),
    NodeId (..),
    ValidGraph,
    Weight (..),
    buildGraph,
    defaultEdgeWeight,
    graphEdges,
    graphNodes,
    isValidNode,
    neighbors,
    nodeCount,
  )

data LoadGraphError
  = LoadReadError FilePath String
  | LoadParseError ParseError
  deriving (Eq, Show)

displayLoadGraphError :: LoadGraphError -> String
displayLoadGraphError err =
  case err of
    LoadReadError path message ->
      "Could not read file "
        ++ path
        ++ ": "
        ++ message
    LoadParseError parseError ->
      displayParseError parseError

loadGraphFile :: FilePath -> IO (Either LoadGraphError ValidGraph)
loadGraphFile path = do
  result <- try (readFile path) :: IO (Either IOException String)
  pure $
    case result of
      Left exception ->
        Left (LoadReadError path (show exception))
      Right contents ->
        case parseGraphFile contents of
          Left parseError -> Left (LoadParseError parseError)
          Right graph -> Right graph

parseGraphFile :: String -> Either ParseError ValidGraph
parseGraphFile =
  finalize <=< foldM step initialState . prepareLines

describeGraph :: ValidGraph -> String
describeGraph graph =
  unlines
    [ "  Nodes:      " ++ show (nodeCount graph),
      "  Edges:      " ++ show (length (graphEdges graph)),
      "",
      adjSummary graph
    ]

validateRunNodes :: ValidGraph -> NodeId -> Maybe NodeId -> Either ParseError ()
validateRunNodes graph source target = do
  validateNode graph CtxSource source
  traverse_ (validateNode graph CtxTarget) target

validateRunNodesForAlgorithm ::
  ValidGraph -> Algorithm -> Maybe NodeId -> Maybe NodeId -> Either ParseError ()
validateRunNodesForAlgorithm graph algo mSource target = do
  traverse_ (validateNode graph CtxTarget) target
  case (algo, mSource) of
    (BFS, Just source) ->
      validateNode graph CtxSource source
    (BellmanFord, Just source) ->
      validateNode graph CtxSource source
    (_, Just source) ->
      validateNode graph CtxSource source
    _ ->
      Right ()

data ParseState = ParseState
  { psNodeCount :: Maybe Int,
    psWeighted :: Bool,
    psInEdges :: Bool,
    psEdges :: [Edge]
  }

initialState :: ParseState
initialState =
  ParseState
    { psNodeCount = Nothing,
      psWeighted = False,
      psInEdges = False,
      psEdges = []
    }

prepareLines :: String -> [String]
prepareLines =
  filter (not . null)
    . map trim
    . lines

step :: ParseState -> String -> Either ParseError ParseState
step st line =
  case words line of
    ["NODES", nStr] -> do
      n <- parsePositive CtxNodes nStr
      Right st {psNodeCount = Just n, psInEdges = False}
    ["EDGES"] ->
      Right st {psInEdges = True}
    ["WEIGHTED"] ->
      Right st {psWeighted = True, psInEdges = False}
    ws | psInEdges st ->
      parseEdgeLine st ws
    _ ->
      Left (UnknownLine line)

parseEdgeLine :: ParseState -> [String] -> Either ParseError ParseState
parseEdgeLine st ws
  | psWeighted st =
      case ws of
        [fromStr, toStr, weightStr] -> do
          from <- parseNodeId CtxEdgeFrom fromStr
          to <- parseNodeId CtxEdgeTo toStr
          weight <- parseWeight CtxEdgeWeight weightStr
          let edge = Edge from to weight
          Right st {psEdges = edge : psEdges st}
        _ -> Left InvalidWeightedEdge
  | otherwise =
      case ws of
        [fromStr, toStr] -> do
          from <- parseNodeId CtxEdgeFrom fromStr
          to <- parseNodeId CtxEdgeTo toStr
          let edge = Edge from to defaultEdgeWeight
          Right st {psEdges = edge : psEdges st}
        [_, _, _] ->
          Left (WeightOnUnweightedGraph (unwords ws))
        _ -> Left InvalidUnweightedEdge

finalize :: ParseState -> Either ParseError ValidGraph
finalize st = do
  nodeTotal <-
    maybe (Left (MissingDirective DirNodes)) Right (psNodeCount st)
  if null (psEdges st)
    then Left NoEdges
    else do
      let edges = reverse (psEdges st)
      firstGraphError (buildGraph nodeTotal edges)

firstGraphError :: Either GraphError ValidGraph -> Either ParseError ValidGraph
firstGraphError (Left err) =
  Left (graphErrorToParseError err)
firstGraphError (Right graph) =
  Right graph

graphErrorToParseError :: GraphError -> ParseError
graphErrorToParseError err =
  case err of
    GraphBuildInvalidNodeCount n ->
      InvalidPositiveInteger CtxNodes (show n)
    GraphBuildInvalidNodeId endpoint nodeId maxNode ->
      NodeOutOfRange (graphEndpointToParseContext endpoint) nodeId maxNode

graphEndpointToParseContext :: GraphEndpoint -> ParseContext
graphEndpointToParseContext endpoint =
  case endpoint of
    EdgeFrom -> CtxEdgeFrom
    EdgeTo -> CtxEdgeTo

validateNode :: ValidGraph -> ParseContext -> NodeId -> Either ParseError ()
validateNode graph ctx nodeId
  | isValidNode graph nodeId = Right ()
  | otherwise =
      Left (NodeOutOfRange ctx nodeId (nodeCount graph - 1))

parsePositive :: ParseContext -> String -> Either ParseError Int
parsePositive ctx raw =
  case readPositiveInt raw of
    Left _ -> Left (InvalidPositiveInteger ctx raw)
    Right n -> Right n

parseWeight :: ParseContext -> String -> Either ParseError Weight
parseWeight ctx raw =
  case readPositiveInt raw of
    Left _ -> Left (InvalidPositiveInteger ctx raw)
    Right n -> Right (Weight n)

parseNodeId :: ParseContext -> String -> Either ParseError NodeId
parseNodeId ctx raw =
  case readNonNegativeInt raw of
    Left _ -> Left (InvalidNodeId ctx raw)
    Right n -> Right (NodeId n)

adjSummary :: ValidGraph -> String
adjSummary graph =
  unlines
    ( "  Adjacency:"
        : map formatAdj (graphNodes graph)
    )
  where
    formatAdj nodeId =
      let nbs = neighbors graph nodeId
       in "    "
            ++ show nodeId
            ++ " -> "
            ++ if null nbs
              then "[]"
              else unwords (map formatNeighbor nbs)
    formatNeighbor (to, weight)
      | weight == defaultEdgeWeight = show to
      | otherwise = show to ++ "(" ++ show weight ++ ")"
