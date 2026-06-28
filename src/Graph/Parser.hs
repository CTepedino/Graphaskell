module Graph.Parser
  ( LoadGraphError (..),
    loadGraphFile,
    parseGraphFile,
    describeGraph,
    validateRunNodes,
    displayLoadGraphError,
  )
where

import Control.Exception (IOException, try)
import Control.Monad ((<=<), foldM)
import Data.Char (isSpace)
import Data.Foldable (traverse_)
import Graph.ParseError
import Graph.Reading (readNonNegativeInt, readPositiveInt)
import Graph.Types (Edge (..), Graph, NodeId (..), Weight (..), buildGraph, edgeWeight, graphEdges, graphNodes, isValidNode, neighbors, nodeCount)

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

loadGraphFile :: FilePath -> IO (Either LoadGraphError Graph)
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

parseGraphFile :: String -> Either ParseError Graph
parseGraphFile =
  finalize <=< foldM step initialState . prepareLines

describeGraph :: Graph -> String
describeGraph graph =
  unlines
    [ "  Nodes:      " ++ show (nodeCount graph),
      "  Edges:      " ++ show (length (graphEdges graph)),
      "",
      adjSummary graph
    ]

validateRunNodes :: Graph -> NodeId -> Maybe NodeId -> Either ParseError ()
validateRunNodes graph source target = do
  validateNode graph CtxSource source
  traverse_ (validateNode graph CtxTarget) target

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

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

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
    ["SOURCE", _] ->
      Left (LegacyCliDirective "SOURCE")
    ["TARGET", _] ->
      Left (LegacyCliDirective "TARGET")
    ["ALGORITHM", _] ->
      Left (LegacyCliDirective "ALGORITHM")
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
          let edge = Edge from to (Just weight)
          Right st {psEdges = edge : psEdges st}
        _ -> Left InvalidWeightedEdge
  | otherwise =
      case ws of
        [fromStr, toStr] -> do
          from <- parseNodeId CtxEdgeFrom fromStr
          to <- parseNodeId CtxEdgeTo toStr
          let edge = Edge from to Nothing
          Right st {psEdges = edge : psEdges st}
        [_, _, _] ->
          Left (WeightOnUnweightedGraph (unwords ws))
        _ -> Left InvalidUnweightedEdge

finalize :: ParseState -> Either ParseError Graph
finalize st = do
  nodeTotal <-
    maybe (Left (MissingDirective DirNodes)) Right (psNodeCount st)
  if null (psEdges st)
    then Left NoEdges
    else do
      let edges = reverse (psEdges st)
          graph = buildGraph nodeTotal edges
      mapM_ (validateEdge graph) edges
      if psWeighted st && any (== Nothing) (map edgeWeight edges)
        then Left WeightedModeMismatch
        else pure graph

validateNode :: Graph -> ParseContext -> NodeId -> Either ParseError ()
validateNode graph ctx nodeId
  | isValidNode graph nodeId = Right ()
  | otherwise =
      Left (NodeOutOfRange ctx nodeId (nodeCount graph - 1))

validateEdge :: Graph -> Edge -> Either ParseError ()
validateEdge graph edge = do
  validateNode graph CtxEdgeFrom (edgeFrom edge)
  validateNode graph CtxEdgeTo (edgeTo edge)

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

adjSummary :: Graph -> String
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
    formatNeighbor (to, Nothing) = show to
    formatNeighbor (to, Just weight) = show to ++ "(" ++ show weight ++ ")"
